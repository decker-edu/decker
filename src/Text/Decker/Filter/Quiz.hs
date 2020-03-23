{-| 
  Module: Quiz
  Description: Provides functionality for creating different types of quiz questions
  Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> 
  Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
  
  This module enables creating different types of quiz questions in decker.
  Currently possible: 
    - Blanktext/Cloze tests
    - Multiple choice
    - Free text questions 
    - Matching/pair questions
-}
module Text.Decker.Filter.Quiz
  ( renderQuizzes
  , renderQuiz
  , quizMap
  ) where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

import Text.Blaze.Html as Blaze
import Text.Blaze.Html.Renderer.String as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Decker
import Text.Decker.Filter.Util as U
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf

quizMap :: Map Text.Text Quiz
quizMap =
  Map.fromList
    [ ("quiz-match-items", Match)
    , ("quiz-mi", Match)
    , ("qmi", Match)
    , ("quiz-multiple-choice", Mult)
    , ("quiz-mc", Mult)
    , ("qmc", Mult)
    , ("quiz-insert-choices", Ins)
    , ("quiz-ic", Ins)
    , ("qic", Ins)
    , ("quiz-free-text", Free)
    , ("quiz-ft", Free)
    , ("qft", Free)
    ]

data Quiz
  = Match
  | Mult
  | Ins
  | Free
  deriving (Show)

helper :: Text.Text -> Html
helper = H.toHtml

renderQuiz :: Quiz -> [Block] -> [Block]
renderQuiz Match b =
  [Div ("", ["quiz-mi", "columns"], []) $ map (renderQuiz_ Match) b]
renderQuiz Mult b =
  [Div ("", ["quiz-mc", "columns"], []) $ map (renderQuiz_ Mult) b]
renderQuiz Ins b =
  [Div ("", ["quiz-ic", "columns"], []) $ map (renderQuiz_ Ins) b]
renderQuiz Free b =
  [Div ("", ["quiz-ft", "columns"], []) $ map (renderQuiz_ Free) b]

renderQuiz_ :: Quiz -> Block -> Block
renderQuiz_ Mult bl@(BulletList blocks) = (mcHtml . quizTaskList Mult) bl
renderQuiz_ Ins bl@(BulletList blocks) = (insertHtml . quizTaskList Ins) bl
renderQuiz_ Free bl@(BulletList blocks) = (freeHtml . quizTaskList Free) bl
renderQuiz_ Match bl@(DefinitionList blocks) = (matchHtml . matchList) bl
renderQuiz_ _ bl = Div ("", ["question", "columns"], []) [bl]

--  the biggest question is how to handle stuff that has been processed already by pandoc
-- e.g. images, links etc
data Match
  = Pair [Inline] [[Block]]
  | Distractor [[Block]]
  deriving (Show)

data Answer =
  Answer
    { correct :: Bool
    , solution :: [Inline]
    , comment :: [Block]
    }
  deriving (Show)

matchList :: Block -> Maybe [(Int, Match)]
matchList (DefinitionList items) =
  Just $ zip [0 .. length items] (map parseDL items)
  where
    parseDL (Str "!":_, bs) = Distractor bs
    parseDL (is, bs) = Pair is bs
matchList b = Nothing

matchHtml :: Maybe [(Int, Match)] -> Block
matchHtml (Just matches) =
  Div ("", ["answers", "columns"], []) [dropzone, dragzone]
  where
    (dropzones, draggables) = unzip $ map pairs matches
    dragzone = Div ("", ["dragzone"], []) (concat draggables)
    dropzone = Div ("", ["dropzones"], []) dropzones
    draggable :: Text.Text -> [Block] -> Block
    draggable index =
      Div ("", ["draggable"], [("draggable", "true"), ("id", index)])
    draggableDist :: Text.Text -> [Block] -> Block
    draggableDist index =
      Div
        ( ""
        , ["draggable", "distractor"]
        , [("draggable", "true"), ("id", index)])
    pairs :: (Int, Match) -> (Block, [Block])
    pairs (i, Distractor bs) =
      (Null, map (draggableDist (Text.pack $ show i)) bs)
    pairs (i, Pair is bs) =
      ( Div ("", ["dropzone"], []) [Plain is]
      , map (draggable (Text.pack $ show i)) bs)
matchHtml Nothing = Plain [Str "NO MATCH"]

-- This TaskList Parsing is potentially dangerous. Does every system know these checkbox symbols?
quizTaskList :: Quiz -> Block -> Maybe [Answer]
quizTaskList q (BulletList blocks) = Just (map (parseTL q) blocks)
  where
    parseTL _ (Plain (Str "☒":Space:is):bs) = Answer True is bs
    parseTL _ (Plain (Str "☐":Space:is):bs) = Answer False is bs
    parseTL Free (Plain is:bs) = Answer True is bs
    parseTL _ is = Answer False [] [Null]
quizTaskList q b = Nothing

-- | This div is hidden 
solutionDiv :: [Answer] -> Block
solutionDiv answers = Div ("", ["solutions"], []) $ map tempName answers
  where
    tempName (Answer _ _ (Null:r)) = Plain [Str "NO TASKLIST ITEM"]
    tempName (Answer True is bs) =
      Div
        ("", ["solution", "correct"], [])
        [Plain is, Div ("", ["tooltip", "correct"], []) bs]
    tempName (Answer False is bs) =
      Div
        ("", ["solution", "wrong"], [])
        [Plain is, Div ("", ["tooltip", "wrong"], []) bs]

insertHtml :: Maybe [Answer] -> Block
insertHtml (Just answers) =
  Div ("", ["answers", "columns"], []) [Plain [insertHtml], solutionDiv answers]
  where
    manageAnswerList = map (\x -> (helper $ stringify $ solution x))
    insertHtml :: Inline
    insertHtml =
      rawHtml $ Text.pack $ S.renderHtml $ options (manageAnswerList answers)
    options xs =
      case xs of
        [x] -> H.input ! A.class_ "blankInput"
        xs ->
          H.select ! A.class_ "blankSelect" $
          (foldr (\x -> (>>) (H.option x)) (H.area) xs)

freeHtml :: Maybe [Answer] -> Block
freeHtml (Just answers) =
  Div ("", ["answers"], []) [Para [inputHtml], solutionDiv answers]
  where
    inputHtml =
      rawHtml $
      Text.pack $
      S.renderHtml $
      H.span $ do
        H.input ! A.class_ "freetextInput"
        H.br
        H.button ! A.class_ "freetextAnswerButton" $ helper "Solution"
freeHtml Nothing = Para [Str "ERROR SOMETHING"]

mcHtml :: Maybe [Answer] -> Block
mcHtml (Just answers) =
  Div
    ("", ["answers", "survey"], [])
    [BulletList (map tempName answers), solutionDiv answers]
  where
    tempName (Answer _ _ (Null:r)) = [Plain [Str "NO TASKLIST ITEM"]]
    tempName (Answer True is bs) =
      [Plain is, Div ("", ["tooltip", "correct"], []) bs]
    tempName (Answer False is bs) =
      [Plain is, Div ("", ["tooltip", "wrong"], []) bs]
mcHtml Nothing = Para [Str "ERROR SOMETHING"]

{-
Pandoc has no actual TaskList data type and parses TLs themselves like this
-- | Convert a list item containing tasklist syntax (e.g. @[x]@)
-- to using @U+2610 BALLOT BOX@ or @U+2612 BALLOT BOX WITH X@.
taskListItemFromAscii :: Extensions -> [Block] -> [Block]
taskListItemFromAscii = handleTaskListItem parseTL
  where
    parseTL (Str "[" : Space : Str "]" : Space : is) = Str "☐" : Space : is
    parseTL (Str "[x]"                 : Space : is) = Str "☒" : Space : is
    parseTL (Str "[X]"                 : Space : is) = Str "☒" : Space : is
    parseTL is = is
-}
-------------------------------------------
-- Old Quiz Stuff
-------------------------------------------
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
      [BulletList (map multipleChoiceHtml blocks), answerButton]
  where
    answerButton =
      Para $
      [LineBreak] ++
      [U.toHtml "<button class=\"mcAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++ [U.toHtml "</button>"]
-- Default pass through
renderMultipleChoice block = block

-- | Renders a freetext question from a bullet list with special syntax
renderFreetextQuestion :: Block -> Block
renderFreetextQuestion bl@(BulletList ((firstBlock:_):(sndBlock:_):_)) =
  case (checkIfFreetextQuestion firstBlock, checkIfFreetextAnswer sndBlock) of
    (Just q, Just a) ->
      Div
        ("", ["freetextQuestion"], [])
        [Para $ freetextQuestionHtml q (Text.unpack (stringify a))]
    _ -> bl
renderFreetextQuestion block = block

-- | Renders a "matching" question from a definition list with special syntax
renderMatching :: Block -> Block
renderMatching dl@(DefinitionList items) =
  case traverse checkIfMatching items of
    Just l -> matchingHtml l
    Nothing -> dl
renderMatching block = block

-- | Renders a "blanktext" Cloze question from a definition list with special syntax
renderBlanktext :: Block -> Block
renderBlanktext dl@(DefinitionList items) =
  case traverse checkIfBlanktext items of
    Just l -> Div ("", [], []) (map blanktextHtml l)
    Nothing -> dl
renderBlanktext block = block

{-
Functions that create lower level Html Elements for the question types using Pandocs Block/Inline Data types
-}
-- | Renders a multiple choice answer 
-- Throws away the identifier and sourrounds the content with a div
-- The div has the class right or wrong according to how it was marked
multipleChoiceHtml :: [Block] -> [Block]
multipleChoiceHtml (prelude:rest) =
  [Div ("", "answer" : cls, []) (prelude' : (map mcTooltipHtml rest))]
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
mcTooltipHtml :: Block -> Block
mcTooltipHtml (BulletList (content:_)) = Div ("", ["tooltip"], []) content
mcTooltipHtml block = block

-- | create the html element for the blanktext question
blanktextHtml :: ([Inline], [Block]) -> Block
blanktextHtml (inlines, blocks) =
  Div ("", ["blankText", "columns"], []) (selects ++ [answerButton])
  where
    title = Header 2 ("", [], []) inlines
    selects = map html blocks
    html (Plain x) = Para (blanktextHtmlAnswers $ splitBlankText x)
    answerButton =
      Para $
      [U.toHtml "<button class=\"btAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++ [U.toHtml "</button>"]
    -- | Split the Blanktext Inline into a List of strings. 
    -- The list elements are either simple text or a String of answer options that gets processed later
    splitBlankText :: [Inline] -> [String]
    splitBlankText inlines =
      concatMap
        (split (startsWith "{"))
        (split (endsWith "}") (Text.unpack (stringify inlines)))

-- | Takes the List of Strings (text + possible answer options) and if it's an answer list generate a dropdown menu
blanktextHtmlAnswers :: [String] -> [Inline]
blanktextHtmlAnswers =
  concatMap
    (\x
                     -- If the answers contain only one element without separators
                     -- Create an HTML input field
      ->
       if "{" `isPrefixOf` x && "}" `isSuffixOf` x && not ("|" `isInfixOf` x)
         then [ U.toHtml
                  (Text.pack
                     (printf
                        "<input type=\"text\" answer=\"%s\" class=\"blankInput\">"
                        (filter (/= '!') . drop 1 . init $ x)))
              ]
                                                                                -- else the answers contain multiple elements separated by "|"
                                                                                -- Create an HTML select element
         else if "{" `isPrefixOf` x && "}" `isSuffixOf` x && "|" `isInfixOf` x
                then [U.toHtml "<select class=\"blankSelect\">"] ++
                     map insertOption (split' x) ++ [U.toHtml "</select>"]
                       -- Else the string is filler text
                else [Str (Text.pack x)])
  where
    split' = splitOn "|" . drop 1 . init
    -- Take an answer option and create an HTML option element. 
    -- If its prefix is "!" it's a correct answer
    insertOption :: String -> Inline
    insertOption ('!':x) =
      U.toHtml
        (Text.pack
           (printf
              "<option class=\"blankOption\" answer=\"true\" value=\"%s\">%s</option>"
              x
              x))
    insertOption x =
      U.toHtml
        (Text.pack
           (printf
              "<option class=\"blankOption\" answer=\"false\" value=\"%s\">%s</option>"
              x
              x))

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
      [U.toHtml "<button class=\"matchingAnswerButton\" type=\"button\">"] ++
      [Str "Show Solution"] ++
      [U.toHtml "</button>"] ++
      [U.toHtml "<button class=\"retryButton\" type=\"button\">"] ++
      [Str "Retry"] ++ [U.toHtml "</button>"]
    wrapDrop :: [[Inline]] -> Block
    wrapDrop inlines = Div ("", ["dropzones"], []) dropzones
      where
        dropzones = (\i -> Div ("", ["dropzone"], []) [Plain i]) <$> inlines

--
freetextQuestionHtml :: [Inline] -> String -> [Inline]
freetextQuestionHtml question answer =
  [U.toHtml "<form onSubmit=\"return false;\">"] ++
  question ++
  [LineBreak] ++
  [ U.toHtml
      (Text.pack
         ("<input type=\"text\" answer=\"" ++
          answer ++ "\" class=\"freetextInput\">"))
  ] ++
    --
  [LineBreak] ++
  [U.toHtml "<button class=\"freetextAnswerButton\" type=\"button\">"] ++
  [Str "Show Solution"] ++ [U.toHtml "</button>"] ++ [U.toHtml "</form>"]

-- | Check if a DefinitionList is a blank text question
checkIfBlanktext :: ([Inline], [[Block]]) -> Maybe ([Inline], [Block])
checkIfBlanktext ([Str "{blanktext}"], firstBlock:_) = Just ([], firstBlock)
checkIfBlanktext (Str "{blanktext}":Space:rest, firstBlock:_) =
  Just (rest, firstBlock)
checkIfBlanktext _ = Nothing

-- | Check if a DefinitionList is a matching question
checkIfMatching :: ([Inline], [[Block]]) -> Maybe ([Inline], [[Block]])
checkIfMatching (Str "{match}":Space:rest, firstBlock:_) =
  Just (rest, [firstBlock])
checkIfMatching _ = Nothing

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
