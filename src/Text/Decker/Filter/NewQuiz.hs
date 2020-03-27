module Text.Decker.Filter.NewQuiz
  ( parseQuizzes
  ) where

import Control.Lens hiding (Choice)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Walk

import qualified Data.Map.Strict as M
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
parseQuizzes :: Pandoc -> Decker Pandoc
parseQuizzes pandoc = return $ walk parseQuizboxes pandoc

parseQuizboxes :: Block -> Block
parseQuizboxes d@(Div (id_, tgs@("box":cls), kvs) blocks)
  | any (`elem` cls) ["qmi", "quiz-mi", "quiz-match-items"] =
    quizDiv defaultMatch
  | any (`elem` cls) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
    quizDiv defaultMC
  | any (`elem` cls) ["qic", "quiz-ic", "quiz-insert-choices"] =
    quizDiv defaultIC
  | any (`elem` cls) ["qft", "quiz-ft", "quiz-free-text"] = quizDiv defaultFree
  | otherwise = d
  where
    quizDiv q =
      Div (id_, tgs, kvs) [Para [Str (parseAndSet (set tags tgs q) blocks)]]
parseQuizboxes bl = bl

defaultMatch = MatchItems [Str "Empty"] [] "" "" [] []

defaultMC = MultipleChoice [Str "Empty"] [] "" "" [] []

defaultIC = InsertChoices [Str "Empty"] [] "" "" []

defaultFree = FreeText [Str "Empty"] [] "" "" [] []

setTags :: Quiz -> [T.Text] -> Quiz
setTags q ts = set tags ts q

-- | Temporary
parseAndSet :: Quiz -> [Block] -> T.Text
parseAndSet q bs = T.pack $ show (foldr parseAndSetQuiz q bs)

-- | This monolithic function parses a Pandoc Block and uses lenses to set the field in the given quiz item
parseAndSetQuiz :: Block -> Quiz -> Quiz
-- Set the title
parseAndSetQuiz (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set the meta information
-- Probably should be outsourced to (a) separate function(s)
parseAndSetQuiz (CodeBlock (id_, cls, kvs) code) q =
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
parseAndSetQuiz (DefinitionList items) quiz@(MatchItems ti ta cat id_ q p) =
  set pairs (map parseDL (zip [0 .. length items] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs
-- parse and set choices for FreeText
parseAndSetQuiz (BulletList blocks) quiz@(FreeText ti ta cat id_ q ch) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- parse and Set choices for InsertChoices
parseAndSetQuiz (BulletList blocks) quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([], map (parseQuizTLItem quiz) blocks) : q) quiz
-- Parse and set choices for MultipleChoice
parseAndSetQuiz (BulletList blocks) quiz@(MultipleChoice ti ta cat id_ q ch) =
  set choices (map (parseQuizTLItem quiz) blocks) quiz
-- Parse and set question for matching
parseAndSetQuiz b quiz@(MatchItems ti ta cat id_ q p) =
  set question (b : q) quiz
-- Parse and set questions for FreeText
parseAndSetQuiz b quiz@(FreeText ti ta cat id_ q ch) = set question (b : q) quiz
-- Set question for InsertChoices
parseAndSetQuiz b quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([b], []) : q) quiz
-- Set question for Multiple Choice
parseAndSetQuiz b quiz@(MultipleChoice ti ta cat id_ q ch) =
  set question (b : q) quiz
-- Default
parseAndSetQuiz _ q = q

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

-- | Meta options for quizzes: score, category, lectureID, topic
setQuizMeta :: Block -> Quiz -> Quiz
setQuizMeta (CodeBlock (id_, cls, kvs) code) q =
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
setQuizMeta _ q = q
