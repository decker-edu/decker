{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> --}
module Quiz
  ( renderQuizzes
  -- , renderQuestions
  ) where

import Common
import Control.Exception
import Debug.Trace as DT
import Filter
import Text.Pandoc
import Text.Pandoc.Walk

-- Render all types of questions
renderQuizzes :: Pandoc -> Decker Pandoc
renderQuizzes pandoc = do
  let mc = walk renderMultipleChoice pandoc
  let match = walk renderMatching mc
  return $ walk renderfreeTextQuestion match

-- 
renderfreeTextQuestion :: Block -> Block
renderfreeTextQuestion bl@(BulletList blocks@((firstBlock:_):(sndBlock:_):_)) =
  case (checkIfFreetextQuestion firstBlock, checkIfFreetextAnswer sndBlock) of
    (Just q, Just a) ->
      Div ("", ["freetextQuestion"], []) [Para $ freetextQuestionHtml q a]
    _ -> bl
renderfreeTextQuestion block = block

-- 
renderMatching :: Block -> Block -- DefinitionList ((rest, blocks) : tail)
-- renderMatching dl@(DefinitionList ((Str "[match]":Space:rest, blocks):tail)) =
renderMatching dl@(DefinitionList items) =
  case traverse checkIfMatching items of
    Just l -> matchingHtml l
    Nothing -> dl
renderMatching block = block

matchingHtml :: [([Inline], [[Block]])] -> Block
matchingHtml dListItems = Div ("", ["matching"], []) [dropzones, dragzone]
  where
    (a, b) = unzip dListItems
    draggable = Div ("", ["draggable"], [("draggable", "true")])
    dragzone = Div ("", ["dragzone"], []) (fmap draggable (concat b))
    dropzones = wrapDrop a

wrapDrop :: [[Inline]] -> Block
wrapDrop inlines = Div ("", ["dropzones"], []) dropzones
  where
    dropzones = (\i -> Div ("", ["dropzone"], []) [Para i]) <$> inlines

checkIfMatching :: ([Inline], [[Block]]) -> Maybe ([Inline], [[Block]])
checkIfMatching (Str "[match]":Space:rest, firstBlock:_) =
  Just (rest, [firstBlock])
checkIfMatching _ = Nothing

-- Utility function
toHtml :: String -> Inline
toHtml = RawInline (Format "html")

-- 
freetextQuestionHtml :: [Inline] -> [Inline] -> [Inline]
freetextQuestionHtml question answer =
  [toHtml "<form onSubmit=\"return false;\">"] ++
  question ++
  [LineBreak] ++
  [toHtml "<input type=\"text\" class=\"freetextInput\">"] ++
  -- 
  [LineBreak] ++
  [toHtml $ "<button class=\"freetextAnswerButton\" type=\"button\">"] ++
  [Str "Answer:"] ++
  [ Span
      ("", ["freetextAnswer"], [("style", "display:none;"), ("type", "text")])
      answer
  ] ++
  [toHtml "</button>"] ++ [toHtml "</form>"]

checkIfFreetextQuestion :: Block -> Maybe [Inline]
checkIfFreetextQuestion (Para (Str "[?]":q)) = Just q
checkIfFreetextQuestion (Plain (Str "[?]":q)) = Just q
checkIfFreetextQuestion _ = Nothing

checkIfFreetextAnswer :: Block -> Maybe [Inline]
checkIfFreetextAnswer (Para (Str "[!]":a)) = Just a
checkIfFreetextAnswer (Plain (Str "[!]":a)) = Just a
checkIfFreetextAnswer _ = Nothing

-- renderMatching :: Block -> Block
-- checkIfMatching :: Block -> Bool
-- checkIfMatching (Para ((Str "["):Inline:Str "]")) = True
-- | Renders a multiple choice question
-- A multiple choice question is a bullet list in the style of a task list.
-- A div class survey is created around the bullet list
renderMultipleChoice :: Block -> Block
-- BulletList which qualifies as survey
renderMultipleChoice (BulletList blocks@((firstBlock:_):_))
  | checkIfMC firstBlock =
    Div ("", ["survey"], []) [BulletList (map renderAnswerMC blocks)]
-- Default pass through
renderMultipleChoice block = block

-- | Checks if a block starts with [X] or [ ] to indicate a survey
checkIfMC :: Block -> Bool
checkIfMC (Para ((Str "[X]"):_)) = True
checkIfMC (Para ((Str "["):Space:(Str "]"):_)) = True
checkIfMC (Plain ((Str "[X]"):_)) = True
checkIfMC (Plain ((Str "["):Space:(Str "]"):_)) = True
checkIfMC (Plain ((Link nullAttr [] ('#':_, "")):Space:_)) = True
checkIfMC _ = False

-- | Renders a multiple choice answer 
-- Throws away the identifier and sourrounds the content with a div
-- The div has the class right or wrong according to how it was marked
renderAnswerMC :: [Block] -> [Block]
renderAnswerMC (prelude:rest) =
  [Div ("", "answer" : cls, []) (prelude' : (map renderTooltipMC rest))]
  where
    (cls, prelude') =
      case prelude of
        Para ((Str "[X]"):prest) -> (["right"], Para prest)
        Para ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        Plain ((Str "[X]"):prest) -> (["right"], Para prest)
        Plain ((Str "["):Space:(Str "]"):prest) -> (["wrong"], Para prest)
        prest -> ([], prest)

-- if there is a bullet list create a div class tooltip around
-- if there are multiple bullet points, all but the first are thrown away
renderTooltipMC :: Block -> Block
renderTooltipMC (BulletList (content:_)) = Div ("", ["tooltip"], []) content
renderTooltipMC block = block
