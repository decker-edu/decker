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
  | any (`elem` cls) ["qmi", "quiz-mi", "quiz-match-items"]
    -- parseMatching blocks 
   =
    Div
      (id_, "box" : cls, kvs)
      [Para [Str (parseAndSet (set tags tgs defaultMatch) blocks)]]
  | any (`elem` cls) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
    Div
      (id_, "box" : cls, kvs)
      [Para [Str (parseAndSet (set tags tgs defaultMC) blocks)]]
  | any (`elem` cls) ["qic", "quiz-ic", "quiz-insert-choices"] =
    Div
      (id_, "box" : cls, kvs)
      [Para [Str (parseAndSet (set tags tgs defaultIC) blocks)]]
  | any (`elem` cls) ["qft", "quiz-ft", "quiz-free-text"] =
    Div
      (id_, "box" : cls, kvs)
      [Para [Str (parseAndSet (set tags tgs defaultFree) blocks)]]
  | otherwise = d
parseQuizboxes bl = bl

defaultMatch = MatchItems [Str "Empty"] [] "" "" [] []

defaultMC = MultipleChoice [Str "Empty"] [] "" "" [] []

defaultIC = InsertChoices [Str "Empty"] [] "" "" []

defaultFree = FreeText [Str "Empty"] [] "" "" [] []

setTags :: Quiz -> [T.Text] -> Quiz
setTags q ts = set tags ts q

parseAndSet :: Quiz -> [Block] -> T.Text
parseAndSet q bs = T.pack $ show $ (foldr parseAndSetQuiz q bs)

parseMatching :: Quiz -> [Block] -> T.Text
parseMatching q bs = T.pack $ show $ (foldr (\b -> parseMatching_ b) q bs)
  -- (foldr (\b -> setQuizHeader b . setQuizMeta b . setMatchList b) q bs)

parseMC :: Quiz -> [Block] -> T.Text
parseMC q bs = T.pack $ show $ (foldr (\b -> parseMC_ b) q bs)

parseIC :: Quiz -> [Block] -> T.Text
parseIC q bs = T.pack $ show $ (foldr (\b -> parseIC_ b) q bs)

parseFree :: Quiz -> [Block] -> T.Text
parseFree q bs = T.pack $ show $ (foldr (\b -> parseFree_ b) q bs)

parseAndSetQuiz :: Block -> Quiz -> Quiz
parseAndSetQuiz (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set meta
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
  set choices (map (parseTL quiz) blocks) quiz
  where
    parseTL _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
    parseTL (FreeText title ts cat qId questions chs) (Plain is:bs) =
      Choice True is bs
    parseTL _ is = Choice False [Str "NoTasklistItem"] [Plain []]
-- parse and Set choices for InsertChoices
parseAndSetQuiz (BulletList blocks) quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([], map parseTL blocks) : q) quiz
  where
    parseTL (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL (Plain (Str "☐":Space:is):bs) = Choice False is bs
    parseTL is = Choice False [Str "NoTasklistItem"] [Plain []]
-- Parse and set choices for MultipleChoice
parseAndSetQuiz (BulletList blocks) quiz@(MultipleChoice ti ta cat id_ q ch) =
  set choices (map parseTL blocks) quiz
  where
    parseTL (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL (Plain (Str "☐":Space:is):bs) = Choice False is bs
    parseTL is = Choice False [Str "NoTasklistItem"] [Plain []]
-- Set question for matching
parseAndSetQuiz b quiz@(MatchItems ti ta cat id_ q p) =
  set question (b : q) quiz
-- Parse and set questions for FreeText
parseAndSetQuiz b quiz@(FreeText ti ta cat id_ q ch) =
  set question (q ++ [b]) quiz
-- Set question for InsertChoices
parseAndSetQuiz b quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([b], []) : q) quiz
-- Set question for Multiple Choice
parseAndSetQuiz b quiz@(MultipleChoice ti ta cat id_ q ch) =
  set question (b : q) quiz
-- Default
parseAndSetQuiz b q = q

-- | Parse and set the Quiz title from the h2 header
setQuizHeader :: Block -> Quiz -> Quiz
setQuizHeader (Header 2 (id_, cls, kvs) text) q = set title text q
setQuizHeader _ q = q

setMatchList :: Block -> Quiz -> Quiz
setMatchList (DefinitionList items) quiz@(MatchItems ti ta cat id_ q p) =
  set pairs (map parseDL (zip [0 .. length items] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs

parseMatchList b q = q

parseMatching_ :: Block -> Quiz -> Quiz
-- Set quiz title
parseMatching_ (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set quiz pairs/Match Items
parseMatching_ (DefinitionList items) quiz@(MatchItems ti ta cat id_ q p) =
  set pairs (map parseDL (zip [0 .. length items] items)) quiz
  where
    parseDL :: (Int, ([Inline], [[Block]])) -> Match
    parseDL (i, (Str "!":_, bs)) = Distractor bs
    parseDL (i, (is, bs)) = Pair i is bs
-- Set meta
parseMatching_ (CodeBlock (id_, cls, kvs) code) q =
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
-- Set question for matching
parseMatching_ b quiz@(MatchItems ti ta cat id_ q p) = set question (b : q) quiz

parseFree_ :: Block -> Quiz -> Quiz
-- Set quiz title
parseFree_ (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set meta
parseFree_ (CodeBlock (id_, cls, kvs) code) q =
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
-- choices
parseFree_ (BulletList blocks) quiz@(FreeText ti ta cat id_ q ch) =
  set choices (map (parseTL quiz) blocks) quiz
  where
    parseTL _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
    parseTL (FreeText title ts cat qId questions chs) (Plain is:bs) =
      Choice True is bs
    parseTL _ is = Choice False [Str "NoTasklistItem"] [Plain []]
-- Set question for Freetext
parseFree_ b quiz@(FreeText ti ta cat id_ q ch) = set question (q ++ [b]) quiz

parseIC_ :: Block -> Quiz -> Quiz
-- Set quiz title
parseIC_ (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set meta
parseIC_ (CodeBlock (id_, cls, kvs) code) q =
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
-- choices
parseIC_ (BulletList blocks) quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([], map parseTL blocks) : q) quiz
  where
    parseTL (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL (Plain (Str "☐":Space:is):bs) = Choice False is bs
    -- parseTL (FreeText title ts cat qId questions chs) (Plain is:bs) =
    --   Choice True is bs
    parseTL is = Choice False [Str "NoTasklistItem"] [Plain []]
-- Set question for Freetext
parseIC_ b quiz@(InsertChoices ti ta cat id_ q) =
  set questions (([b], []) : q) quiz

parseMC_ :: Block -> Quiz -> Quiz
-- Set quiz title
parseMC_ (Header 2 (id_, cls, kvs) text) q = set title text q
-- Set quiz pairs/Match Items
parseMC_ (BulletList blocks) quiz@(MultipleChoice ti ta cat id_ q ch) =
  set choices (map parseTL blocks) quiz
  where
    parseTL (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL (Plain (Str "☐":Space:is):bs) = Choice False is bs
    -- parseTL (FreeText title ts cat qId questions chs) (Plain is:bs) =
      -- Choice True is bs
    parseTL is = Choice False [Str "NoTasklistItem"] [Plain []]
-- Set meta
parseMC_ (CodeBlock (id_, cls, kvs) code) q =
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
-- Set question for matching
parseMC_ b quiz@(MultipleChoice ti ta cat id_ q ch) = set question (b : q) quiz
parseMC_ b q = q

parseQuizTaskList :: Quiz -> Block -> Either Block [Choice]
parseQuizTaskList q (BulletList blocks) = Right (map (parseTL q) blocks)
  where
    parseTL _ (Plain (Str "☒":Space:is):bs) = Choice True is bs
    parseTL _ (Plain (Str "☐":Space:is):bs) = Choice False is bs
    parseTL (FreeText title ts cat qId questions chs) (Plain is:bs) =
      Choice True is bs
    parseTL _ is = Choice False [Str "NoTasklistItem"] [Plain []]
parseQuizTaskList q b = Left b

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
