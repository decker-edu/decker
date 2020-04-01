module Text.Decker.Filter.NewQuiz
  ( handleQuizzes
  ) where

import Control.Lens hiding (Choice)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Definition
import Text.Pandoc.Walk

import qualified Data.Map.Strict as M
import Text.Decker.Filter.Local
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

-- Pair: consisting of a bucket where items should be dropped; The items which belong to the bucket
-- Distractor: Just a list of items without accompanying bucket
data Match
  = Pair
      { bucketID :: Int
      , bucket :: [Inline]
      , items :: [[Block]]
      }
  | Distractor
      { items :: [[Block]]
      }
  deriving (Show)

-- | A Choice consists of a Boolean (correct), the answer text and a tooltip comment
data Choice =
  Choice
    { correct :: Bool
    , text :: [Inline]
    , comment :: [Block]
    }
  deriving (Show)

-- | The Quiz datatype.
-- Each quiz type contains: title, tags/classes, category and id.
data Quiz
  = MultipleChoice
  -- Multiple Choice questions consist of one question (e.g. h2 header and some blocks) and a following choices/selection part
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _lectureId :: T.Text
      , _score :: T.Text
      , _topic :: T.Text
      , _question :: [Block]
      , _choices :: [Choice]
      }
  | MatchItems
  -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _lectureId :: T.Text
      , _score :: T.Text
      , _topic :: T.Text
      , _question :: [Block]
      , _pairs :: [Match]
      }
  | InsertChoices
  -- These questions can have multiple question and answer/choices parts. 
  -- This is why questions is a list of tuples. 
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _lectureId :: T.Text
      , _score :: T.Text
      , _topic :: T.Text
      , _questions :: [([Block], [Choice])]
      }
  | FreeText
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _lectureId :: T.Text
      , _score :: T.Text
      , _topic :: T.Text
      , _question :: [Block]
      , _choices :: [Choice]
      }
  deriving (Show)

makeLenses ''Quiz

-- | Has to be called in the Markdown.hs deckerPipeline after processSlides
-- | Depends on h2 headers being wrapped in boxes
handleQuizzes :: Pandoc -> Decker Pandoc
handleQuizzes pandoc = return $ walk parseQuizboxes pandoc
  where
    parseQuizboxes :: Block -> Block
    parseQuizboxes d@(Div (id_, tgs@("box":cls), kvs) blocks)
      | any (`elem` cls) ["qmi", "quiz-mi", "quiz-match-items"] =
        renderQuizzes (parseAndSetQuiz (set tags tgs defaultMatch) blocks)
      | any (`elem` cls) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
        renderQuizzes (parseAndSetQuiz (set tags tgs defaultMC) blocks)
      | any (`elem` cls) ["qic", "quiz-ic", "quiz-insert-choices"] =
        renderQuizzes (parseAndSetQuiz (set tags tgs defaultIC) blocks)
      | any (`elem` cls) ["qft", "quiz-ft", "quiz-free-text"] =
        renderQuizzes (parseAndSetQuiz (set tags tgs defaultFree) blocks)
      | otherwise = d
    parseQuizboxes bl = bl
    -- Give the tag-/classlist of the surrounding div box to the quiz
    setTags :: Quiz -> [T.Text] -> Quiz
    setTags q ts = set tags ts q
    -- The default "new" quizzes
    defaultMatch = MatchItems [Str "Empty"] [] "" "" "" "" [] []
    defaultMC = MultipleChoice [Str "Empty"] [] "" "" "" "" [] []
    defaultIC = InsertChoices [Str "Empty"] [] "" "" "" "" []
    defaultFree = FreeText [Str "Empty"] [] "" "" "" "" [] []

-- Take the parsed Quizzes and render them to html
renderQuizzes :: Quiz -> Block
renderQuizzes quiz@MatchItems {} = renderMatching quiz
renderQuizzes quiz@FreeText {} = renderFreeText quiz
renderQuizzes quiz@InsertChoices {} = renderInsertChoices quiz
renderQuizzes quiz@MultipleChoice {} = renderMultipleChoice quiz

-- Takes a Quiz and a list of blocks, parses the blocks and modifies the given quiz
parseAndSetQuiz :: Quiz -> [Block] -> Quiz
parseAndSetQuiz q bs = combineICQuestions (foldl parseAndSetQuizFields q bs)

-- | This function combines the (Question, Choices) tuples of InsertChoice questions
-- These tuples are of type ([Block], [Choice])
-- This is because in `parseAndSetQuizFields` they are created alternatingly as (bs, []) and ([],chs)
-- This combine function makes it so that the questions field in InsertChoice is 
-- an alternating list of "Question text -> quiz element -> question text" etc
combineICQuestions :: Quiz -> Quiz
combineICQuestions quiz@(InsertChoices ti tgs cat lId sc tpc q) =
  set questions (combineQTuples q) quiz
    -- These tuples can only be ([],a) or (a,[]) per default
    -- They're created like this in parseAndSetQuizFields
  where
    combineQTuples :: [([Block], [Choice])] -> [([Block], [Choice])]
    combineQTuples [] = []
    combineQTuples bc@[(a, b)] = bc
    -- combine two question blocks
    combineQTuples ((a, []):(b, []):rest) = combineQTuples ((a ++ b, []) : rest)
    -- combine Question with a choice block
    combineQTuples ((a, []):([], b):rest) = (a, b) : combineQTuples rest
    -- If the head has anything other than an empty choice then ignore it
    combineQTuples (a:(y, b):rest) = a : combineQTuples ((y, b) : rest)
combineICQuestions q = q

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuizFields :: Quiz -> Block -> Quiz
-- Set the title
parseAndSetQuizFields q (Header 2 (id_, cls, kvs) text) = set title text q
-- Set the meta information
parseAndSetQuizFields q (CodeBlock (id_, cls, kvs) code) =
  if "yaml" `elem` cls
    then (setCategory . setLectureID . setScore . setTopic) q
    else q
  where
    meta :: Meta
    meta = decodeYaml code
    setCategory :: Quiz -> Quiz
    setCategory q =
      case getMetaString "category" meta of
        Just s -> set category (T.pack s) q
        Nothing -> q
    setScore :: Quiz -> Quiz
    setScore q =
      case getMetaString "score" meta of
        Just s -> set score (T.pack s) q
        Nothing -> q
    setTopic :: Quiz -> Quiz
    setTopic q =
      case getMetaString "topic" meta of
        Just s -> set topic (T.pack s) q
        Nothing -> q
    setLectureID :: Quiz -> Quiz
    setLectureID q =
      case getMetaString "lectureId" meta of
        Just s -> set lectureId (T.pack s) q
        Nothing -> q
    decodeYaml :: T.Text -> Meta
    decodeYaml text =
      case decodeEither' (encodeUtf8 text) of
        Right a -> toPandocMeta a
        Left exception -> Meta M.empty
-- Set quiz pairs/Match Items
parseAndSetQuizFields quiz@MatchItems {} (DefinitionList items) =
  set pairs (map parseDL (zip [1 .. length items + 1] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs
-- parse and set choices for FreeText
parseAndSetQuizFields quiz@FreeText {} (BulletList blocks) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- parse and Set choices for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs cat lId sc tpc q) (BulletList blocks) =
  set questions (q ++ [([], map (parseQuizTLItem quiz) blocks)]) quiz
-- Parse and set choices for MultipleChoice
parseAndSetQuizFields quiz@MultipleChoice {} (BulletList blocks) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- The questions are prepended
-- Parse and set question for matching
parseAndSetQuizFields quiz@(MatchItems ti tgs cat lId sc tpc q p) b =
  set question (q ++ [b]) quiz
-- Parse and set questions for FreeText
parseAndSetQuizFields quiz@(FreeText ti tgs cat lId sc tpc q ch) b =
  set question (q ++ [b]) quiz
-- Set question for InsertChoices
parseAndSetQuizFields quiz@(InsertChoices ti tgs cat lId sc tpc q) b =
  set questions (q ++ [([b], [])]) quiz
-- Set question for Multiple Choice
parseAndSetQuizFields quiz@(MultipleChoice ti tgs cat lId sc tpc q ch) b =
  set question (q ++ [b]) quiz

parseQuizTLItem :: Quiz -> [Block] -> Choice
parseQuizTLItem _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
parseQuizTLItem _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
parseQuizTLItem FreeText {} (Plain is:bs) = Choice True is bs
parseQuizTLItem _ is = Choice False [Str "NoTasklistItem"] [Plain []]

solutionButton =
  rawHtml' $ do
    H.br
    H.button ! A.class_ "solutionButton" $ H.toHtml ("Show Solution" :: T.Text)

renderMultipleChoice :: Quiz -> Block
renderMultipleChoice quiz@(MultipleChoice title tgs cat lId sc tpc q ch) =
  Div
    ( ""
    , tgs
    , [("category", cat), ("lectureId", lId), ("score", sc), ("topic", tpc)]) $
  [Header 2 ("", [], []) title] ++ q ++ [choiceBlock] ++ [solutionButton]
  where
    choiceBlock = rawHtml' choiceList
    choiceList :: Html
    choiceList =
      H.ul ! A.class_ "choices" $ foldr ((>>) . handleChoices) H.br ch
    reduceTooltip :: [Block] -> [Block]
    reduceTooltip [BulletList blocks] = concat blocks
    reduceTooltip bs = bs
    handleChoices :: Choice -> Html
    handleChoices (Choice correct text comment) =
      if correct
        then H.li ! A.class_ "correct" $ do
               toHtml text
               H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)
        else H.li ! A.class_ "wrong" $ do
               toHtml text
               H.div ! A.class_ "tooltip" $ toHtml (reduceTooltip comment)
renderMultipleChoice q =
  Div ("", [], []) [Para [Str "ERROR NO MULTIPLE CHOICE QUIZ"]]

renderInsertChoices :: Quiz -> Block
renderInsertChoices quiz@(InsertChoices title tgs cat lId sc tpc q) =
  Div
    ( ""
    , tgs
    , [("category", cat), ("lectureId", lId), ("score", sc), ("topic", tpc)]) $
  [Header 2 ("", [], []) title] ++ questionBlocks q ++ [solutionButton]
  where
    questionBlocks :: [([Block], [Choice])] -> [Block]
    questionBlocks = map (rawHtml' . handleTuple)
    handleTuple :: ([Block], [Choice]) -> Html
    handleTuple ([], [c]) = input c
    handleTuple ([], chs) = select chs
    handleTuple (bs, []) = toHtml (map reduceBlock bs)
    handleTuple (bs, [c]) = toHtml (map reduceBlock bs) >> input c
    handleTuple (bs, chs) = toHtml (map reduceBlock bs) >> select chs
    reduceBlock :: Block -> Block
    reduceBlock (Para is) = Plain ([Str " "] ++ is ++ [Str " "])
    reduceBlock p = p
    input :: Choice -> Html
    input (Choice correct text comment) = H.input
    select :: [Choice] -> Html
    select choices = H.select (foldr ((>>) . options) H.br choices)
    options :: Choice -> Html
    options (Choice correct text comment) =
      if correct
        then H.option ! A.class_ "correct" $ toHtml text
        else H.option ! A.class_ "wrong" $ toHtml text
renderInsertChoices q =
  Div ("", [], []) [Para [Str "ERROR NO INSERT CHOICES QUIZ"]]

-- 
renderMatching :: Quiz -> Block
renderMatching quiz@(MatchItems title tgs cat lId sc tpc qs matches) =
  Div
    ( ""
    , tgs
    , [("category", cat), ("lectureId", lId), ("score", sc), ("topic", tpc)])
    [Header 2 ("", [], []) title, bucketsDiv, itemsDiv, solutionButton]
  where
    (buckets, items) = unzip $ map pairs matches
    itemsDiv = Div ("", ["matchItems"], []) (concat items)
    bucketsDiv = Div ("", ["buckets"], []) buckets
    item :: T.Text -> [Block] -> Block
    item index =
      Div ("", ["matchItem"], [("draggable", "true"), ("bucketId", index)])
    distractor :: [Block] -> Block
    distractor = Div ("", ["matchItem", "distractor"], [("draggable", "true")])
    pairs :: Match -> (Block, [Block])
    pairs (Distractor bs) = (Text.Pandoc.Definition.Null, map distractor bs)
    pairs (Pair i is bs) =
      ( Div ("", ["bucket"], [("bucketId", T.pack $ show i)]) [Plain is]
      , map (item (T.pack $ show i)) bs)
renderMatching q = Div ("", [], []) [Para [Str "ERROR NO MATCHING QUIZ"]]

renderFreeText :: Quiz -> Block
renderFreeText quiz@(FreeText title tgs cat lId sc tpc q ch) =
  Div
    ( ""
    , tgs
    , [("category", cat), ("lectureId", lId), ("score", sc), ("topic", tpc)]) $
  [Header 2 ("", [], []) title] ++ q ++ [inputRaw] ++ [solutionButton]
  where
    inputRaw = rawHtml' H.input
    input :: Choice -> Html
    input (Choice correct text comment) = H.input
renderFreeText q = Div ("", [], []) [Para [Str "ERROR NO FREETEXT QUIZ"]]
