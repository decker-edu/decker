module Text.Decker.Filter.NewQuiz
  ( handleQuizzes
  ) where

import Control.Lens hiding (Choice)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml
import Text.Blaze.Html as Blaze
import Text.Blaze.Html.Renderer.String as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Definition
import Text.Pandoc.Shared
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
      , _qId :: T.Text
      , _question :: [Block]
      , _choices :: [Choice]
      }
  | MatchItems
  -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _qId :: T.Text
      , _question :: [Block]
      , _pairs :: [Match]
      }
  | InsertChoices
  -- These questions can have multiple question and answer/choices parts. 
  -- This is why questions is a list of tuples. 
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _qId :: T.Text
      , _questions :: [([Block], [Choice])]
      }
  | FreeText
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _qId :: T.Text
      , _question :: [Block]
      , _choices :: [Choice]
      }
  deriving (Show)

makeLenses ''Quiz

-- | A Match can be either a
quizClasses :: [T.Text]
quizClasses =
  [ "quiz-match-items"
  , "quiz-mi"
  , "qmi"
  , "quiz-multiple-choice"
  , "quiz-mc"
  , "qmc"
  , "quiz-insert-choices"
  , "quiz-ic"
  , "qic"
  , "quiz-free-text"
  , "quiz-ft"
  , "qft"
  ]

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
    defaultMatch = MatchItems [Str "Empty"] [] "" "" [] []
    defaultMC = MultipleChoice [Str "Empty"] [] "" "" [] []
    defaultIC = InsertChoices [Str "Empty"] [] "" "" []
    defaultFree = FreeText [Str "Empty"] [] "" "" [] []

-- Take the parsed Quizzes and render them to html
renderQuizzes :: Quiz -> Block
renderQuizzes quiz@(MatchItems ti tgs cat id_ q p) =
  Div ("", tgs, []) [Para [Str (T.pack $ show quiz)]]
renderQuizzes quiz@(FreeText ti tgs cat id_ q ch) =
  Div ("", tgs, []) [Para [Str (T.pack $ show quiz)]]
renderQuizzes quiz@(InsertChoices ti tgs cat id_ q) =
  Div ("", tgs, []) [Para [Str (T.pack $ show quiz)]]
renderQuizzes quiz@(MultipleChoice ti tgs cat id_ q ch) =
  renderMultipleChoice quiz
  -- Div ("", tgs, []) [Para [Str (T.pack $ show quiz)]]

-- Takes a Quiz and a list of blocks, parses the blocks and modifies the given quiz
parseAndSetQuiz :: Quiz -> [Block] -> Quiz
parseAndSetQuiz q bs = combineICQuestions (foldr parseAndSetQuizFields q bs)

-- | This function combines the (Question, Choices) tuples of InsertChoice questions
-- These tuples are of type ([Block], [Choice])
-- This is because in `parseAndSetQuizFields` they are created alternatingly as (bs, []) and ([],chs)
-- This combine function makes it so that the questions field in InsertChoice is 
-- an alternating list of "Question text -> quiz element -> question text" etc
combineICQuestions :: Quiz -> Quiz
combineICQuestions quiz@(InsertChoices ti tgs cat id_ q) =
  set questions (combineQTuples q) quiz
    -- These tuples can only be ([],a) or (a,[]) per default
    -- They're created like this in parseAndSetQuizFields
  where
    combineQTuples :: [([Block], [Choice])] -> [([Block], [Choice])]
    combineQTuples bc@[(a, b)] = bc
    -- combine two question blocks
    combineQTuples ((a, []):(b, []):rest) = combineQTuples ((a ++ b, []) : rest)
    -- combine Question with a choice block
    combineQTuples ((a, []):([], b):rest) = (a, b) : (combineQTuples rest)
    -- If the head has anything other than an empty choice then ignore it
    combineQTuples (a:(y, b):rest) = a : combineQTuples ((y, b) : rest)
combineICQuestions q = q

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuizFields :: Block -> Quiz -> Quiz
-- Set the title
parseAndSetQuizFields (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set the meta information
-- Probably should be outsourced to (a) separate function(s)
parseAndSetQuizFields (CodeBlock (id_, cls, kvs) code) q =
  if "yaml" `elem` cls
    then (setCategory . setLectureID) q
    else q
  where
    meta = decodeYaml code
    quizMetaOptions :: [String]
    quizMetaOptions = ["category", "score", "lectureID", "topic"]
    setCategory :: Quiz -> Quiz
    setCategory q =
      case getMetaString "category" meta of
        Just s -> set category (T.pack s) q
        Nothing -> q
    -- setScore :: Quiz -> Meta -> Quiz
    -- setScore q =
    --   case getMetaString "score" meta of
    --     Just s -> set score (T.pack s) q
    --     Nothing -> q
    setLectureID :: Quiz -> Quiz
    setLectureID q =
      case getMetaString "lectureId" meta of
        Just s -> set qId (T.pack s) q
        Nothing -> q
    decodeYaml :: T.Text -> Meta
    decodeYaml text =
      case decodeEither' (encodeUtf8 text) of
        Right a -> toPandocMeta a
        Left exception -> Meta M.empty
-- Set quiz pairs/Match Items
parseAndSetQuizFields (DefinitionList items) quiz@(MatchItems ti ta cat id_ q p) =
  set pairs (map parseDL (zip [0 .. length items] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs
-- parse and set choices for FreeText
parseAndSetQuizFields (BulletList blocks) quiz@(FreeText ti ta cat id_ q ch) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- parse and Set choices for InsertChoices
parseAndSetQuizFields (BulletList blocks) quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([], map (parseQuizTLItem quiz) blocks) : q) quiz
-- Parse and set choices for MultipleChoice
parseAndSetQuizFields (BulletList blocks) quiz@(MultipleChoice ti ta cat id_ q ch) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- Parse and set question for matching
parseAndSetQuizFields b quiz@(MatchItems ti ta cat id_ q p) =
  set question (b : q) quiz
-- Parse and set questions for FreeText
parseAndSetQuizFields b quiz@(FreeText ti ta cat id_ q ch) =
  set question (b : q) quiz
-- Set question for InsertChoices
parseAndSetQuizFields b quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([b], []) : q) quiz
-- Set question for Multiple Choice
parseAndSetQuizFields b quiz@(MultipleChoice ti ta cat id_ q ch) =
  set question (b : q) quiz
-- Default
parseAndSetQuizFields _ q = q

parseQuizTLItem :: Quiz -> [Block] -> Choice
parseQuizTLItem _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
parseQuizTLItem _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
parseQuizTLItem (FreeText title ts cat qId questions chs) (Plain is:bs) =
  Choice True is bs
parseQuizTLItem _ is = Choice False [Str "NoTasklistItem"] [Plain []]

-- | Parse and set the Quiz title from the h2 header
setQuizHeader :: Block -> Quiz -> Quiz
setQuizHeader (Header 2 (id_, cls, kvs) text) q = set title text q
setQuizHeader _ q = q

renderMultipleChoice :: Quiz -> Block
renderMultipleChoice quiz@(MultipleChoice title tgs cat id_ q ch) =
  Div ("", tgs, [("category", cat), ("qID", id_)]) $
  [Para title] ++ q ++ [choiceBlock]
  where
    choiceBlock = rawHtml $ T.pack $ S.renderHtml $ choiceList
    choiceList =
      H.ul ! A.class_ "choices" $ (foldr (\x -> (>>) (handleChoices x)) H.br ch)
    reduceTooltip :: [Block] -> [Block]
    reduceTooltip [BulletList blocks] = concat blocks
    reduceTooltip bs = bs
    handleChoices :: Choice -> Html
    handleChoices (Choice correct text comment) =
      if correct
        then H.li ! A.class_ "correct" $ do
               inlinesToHtml' text
               H.div ! A.class_ "tooltip" $
                 blocksToHtml' (reduceTooltip comment)
        else H.li ! A.class_ "wrong" $ do
               inlinesToHtml' text
               H.div ! A.class_ "tooltip" $
                 blocksToHtml' (reduceTooltip comment)
renderMultipleChoice q =
  Div ("", [], []) [Para [Str "ERROR NO MULTIPLE CHOICE"]]
