{-| 
  Module: Quiz
  Description: Provides functionality for creating different types of quiz questions
  Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> 
  Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
  
  This module enables creating different types of quiz questions in decker.
  Currently possible: 
    - Multiple choice
    - Free text questions 
    - Matching/pair questions
-}
module Text.Decker.Filter.Quiz
  ( renderQuizzes
  ) where

import Data.List
import Data.List.Split
import Text.Decker.Internal.Common

import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Regex.TDFA

-- | Render all types of questions
renderQuizzes :: Pandoc -> Decker Pandoc
renderQuizzes pandoc = do
  let mc = walk renderMultipleChoice pandoc
  let match = walk renderMatching mc
  let blank = walk renderBlanktext match
  return $ walk renderFreetextQuestion blank

-- | Renders a multiple choice question
-- A multiple choice question is a bullet list in the style of a task list.
-- A div class survey is created around the bullet list
renderMultipleChoice :: Block -> Block
-- BulletList which qualifies as survey
renderMultipleChoice (BulletList blocks@((firstBlock:_):_))
  | checkIfMC firstBlock =
    Div
      ("", ["survey"], [])
      [BulletList (map renderAnswerMC blocks), answerButton]
  where
    answerButton =
      Para $
      [LineBreak] ++
      [toHtml "<button class=\"mcAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++ [toHtml "</button>"]
-- Default pass through
renderMultipleChoice block = block

-- | Renders a freetext question from a bullet list with special syntax
renderFreetextQuestion :: Block -> Block
renderFreetextQuestion bl@(BulletList ((firstBlock:_):(sndBlock:_):_)) =
  case (checkIfFreetextQuestion firstBlock, checkIfFreetextAnswer sndBlock) of
    (Just q, Just a) ->
      Div ("", ["freetextQuestion"], []) [Para $ freetextQuestionHtml q a]
    _ -> bl
renderFreetextQuestion block = block

-- | Renders a "matching" question from a definition list with special syntax
renderMatching :: Block -> Block
renderMatching dl@(DefinitionList items) =
  case traverse checkIfMatching items of
    Just l -> matchingHtml l
    Nothing -> dl
renderMatching block = block

-- | Renders a "blanktext" question from a definition list with special syntax
renderBlanktext :: Block -> Block
renderBlanktext dl@(DefinitionList items) =
  case traverse checkIfBlanktext items of
    Just l -> blanktextHtml l
    Nothing -> dl
renderBlanktext block = block

-- | create the html element for the blanktext question
blanktextHtml :: [([Inline], [Block])] -> Block
blanktextHtml dListItems = Div ("", ["blanktext"], []) test
  where
    (inlines, blocks) = unzip dListItems
    test = map html (concat blocks)
    html (Plain x) = Para (generateDropdown $ splitBlankText x)

-- | Split the Blanktext Inline into a List of strings. 
-- The list elements are either simple text or a String of answer options that gets processed later
splitBlankText :: [Inline] -> [String]
splitBlankText inlines =
  concatMap (split (startsWith "{")) (split (endsWith "}") (stringify inlines))

-- | Takes the List of Strings (text + possible answer options) and if it's a answer list generate a dropdown menu
generateDropdown :: [String] -> [Inline]
generateDropdown =
  concatMap
    (\x ->
       if "{" `isPrefixOf` x
         then [toHtml "<select>"] ++
              map insertOption (split' x) ++ [toHtml "</select>"]
         else [Str x])
  where
    split' = splitOn "|" . drop 1 . init
    insertOption :: String -> Inline
    insertOption x = toHtml (printf "<option value=\"%s\">%s</option>" x x)

{-
this is a test string {asdf} and it continues {a bit}

after Pandoc:
[Str "this",Space,Str "is",Space,Str "a",Space,Str "test",Space,Str "string",Space,Str "{asdf}",Space,Str "and",Space,Str "it",Space,Str "continues",Space,Str "{a",Space,Str "bit}"]

- go over list
- check if it's Str
- if Str starts with "{"
- if Str contains "}"
- if Str contains both return Str

takeWhile Str beginnt nicht mit "{"

-}
{- | Blank text questions. Possible syntax

{blanktext}
: Decker is a software built using the programming language {Haskell|Scala|Java} and builds upon {RevealJS|PowerPoint}.

-}
checkIfBlanktext :: ([Inline], [[Block]]) -> Maybe ([Inline], [Block])
checkIfBlanktext (Str "{blanktext}":Space:rest, firstBlock:_) =
  Just (rest, firstBlock)
checkIfBlanktext _ = Nothing

-- | Creates the html representation for a matching question
matchingHtml :: [([Inline], [[Block]])] -> Block
matchingHtml dListItems =
  Div ("", ["matching"], []) [dropzones, dragzone, answerButton]
  where
    (inlines, blocks) = unzip dListItems
    draggable = Div ("", ["draggable"], [("draggable", "true")])
    dragzone = Div ("", ["dragzone"], []) (fmap draggable (concat blocks))
    dropzones = wrapDrop inlines
    answerButton =
      Para $
      [LineBreak] ++
      [toHtml "<button class=\"matchingAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++
      [toHtml "</button>"] ++
      [toHtml "<button class=\"retryButton\" type=\"button\">"] ++
      [Str "Retry"] ++ [toHtml "</button>"]

wrapDrop :: [[Inline]] -> Block
wrapDrop inlines = Div ("", ["dropzones"], []) dropzones
  where
    dropzones = (\i -> Div ("", ["dropzone"], []) [Plain i]) <$> inlines

-- 
freetextQuestionHtml :: [Inline] -> [Inline] -> [Inline]
freetextQuestionHtml question answer =
  [toHtml "<form onSubmit=\"return false;\">"] ++
  question ++
  [LineBreak] ++
  [toHtml "<input type=\"text\" class=\"freetextInput\">"] ++
  -- 
  [LineBreak] ++
  [toHtml "<button class=\"freetextAnswerButton\" type=\"button\">"] ++
  [Str "Show Solution"] ++
  [ Span
      ( ""
      , ["freetextAnswer"]
      , [("style", "display:none; font-color:black;"), ("type", "text")])
      answer
  ] ++
  [toHtml "</button>"] ++ [toHtml "</form>"]

checkIfMatching :: ([Inline], [[Block]]) -> Maybe ([Inline], [[Block]])
checkIfMatching (Str "{match}":Space:rest, firstBlock:_) =
  Just (rest, [firstBlock])
checkIfMatching _ = Nothing

-- 
checkIfFreetextQuestion :: Block -> Maybe [Inline]
checkIfFreetextQuestion (Para (Str "{?}":q)) = Just q
checkIfFreetextQuestion (Plain (Str "{?}":q)) = Just q
checkIfFreetextQuestion _ = Nothing

checkIfFreetextAnswer :: Block -> Maybe [Inline]
checkIfFreetextAnswer (Para (Str "{!}":a)) = Just a
checkIfFreetextAnswer (Plain (Str "{!}":a)) = Just a
checkIfFreetextAnswer _ = Nothing

-- | Checks if a block starts with [X] or [ ] to indicate a survey
checkIfMC :: Block -> Bool
checkIfMC (Para ((Str "{X}"):_)) = True
checkIfMC (Para ((Str "{"):Space:(Str "}"):_)) = True
checkIfMC (Plain ((Str "{X}"):_)) = True
checkIfMC (Plain ((Str "{"):Space:(Str "}"):_)) = True
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
        Para ((Str "{X}"):prest) -> (["right"], Para prest)
        Para ((Str "{"):Space:(Str "}"):prest) -> (["wrong"], Para prest)
        Plain ((Str "{X}"):prest) -> (["right"], Para prest)
        Plain ((Str "{"):Space:(Str "}"):prest) -> (["wrong"], Para prest)
        prest -> ([], prest)

-- if there is a bullet list create a div class tooltip around
-- if there are multiple bullet points, all but the first are thrown away
renderTooltipMC :: Block -> Block
renderTooltipMC (BulletList (content:_)) = Div ("", ["tooltip"], []) content
renderTooltipMC block = block

-- Utility function
toHtml :: String -> Inline
toHtml = RawInline (Format "html")
