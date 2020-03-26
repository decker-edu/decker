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
    , text :: [Block]
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
parseQuizboxes d@(Div (id_, "box":cls, kvs) blocks)
  | any (`elem` cls) ["qmi", "quiz-mi", "quiz-match-items"]
    -- parseMatching blocks 
   =
    Div (id_, "box" : cls, kvs) [Para [Str (parseMatching defaultMatch blocks)]]
  | otherwise = d
parseQuizboxes bl = bl

defaultMatch = MatchItems [Str "Empty"] [] "" "" [Plain [Str "Empty"]] []

setTags :: Quiz -> [T.Text] -> Quiz
setTags q ts = set tags ts q

parseMatching :: Quiz -> [Block] -> T.Text
parseMatching q bs =
  T.pack $ show (foldr (\b -> setQuizHeader b . setQuizMeta b) q bs)

-- parseMatching q [] = T.pack $ show q
-- parseMatching q (b:bs) = parseMatching (setQuizMeta q b) bs
-- | Parse and set the Quiz title from the h2 header
setQuizHeader :: Block -> Quiz -> Quiz
setQuizHeader (Header 2 (id_, cls, kvs) text) q = set title text q
setQuizHeader _ q = q

setMatchList :: Quiz -> Block -> Quiz
setMatchList quiz@(MatchItems ti ta cat id_ q p) (DefinitionList items) =
  set pairs (map parseDL (zip [0 .. length items] items)) quiz
  where
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs

parseMatchList q b = q

-- parseQuizTaskList :: Quiz -> Block
-- quizTaskList :: Quiz -> Block -> Maybe [Answer]
-- quizTaskList q (BulletList blocks) = Just (map (parseTL q) blocks)
--   where
--     parseTL _ (Plain (Str "☒":Space:is):bs) = Answer True is bs
--     parseTL _ (Plain (Str "☐":Space:is):bs) = Answer False is bs
--     parseTL Free (Plain is:bs) = Answer True is bs
--     parseTL _ is = Answer False [] [Null]
-- quizTaskList q b = Nothing
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
