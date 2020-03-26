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

import Text.Decker.Internal.Common

-- Pair: consisting of a bucket where items should be dropped; The items which belong to the bucket
-- Distractor: Just a list of items without accompanying bucket
data Match
  = Pair
      { bucket :: [Inline]
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
      , _id :: T.Text
      , _question :: [Block]
      , _choices :: [Choice]
      }
  | MatchItems
  -- Matching Questions consist of one question and a pairing "area" for sorting items via dragging and dropping
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _id :: T.Text
      , _question :: [Block]
      , _pairs :: [Match]
      }
  | InsertChoices
  -- These questions can have multiple question and answer/choices parts. 
  -- This is why questions is a list of tuples. 
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _id :: T.Text
      , _questions :: [([Block], [Choice])]
      }
  | FreeText
      { _title :: [Inline]
      , _tags :: [T.Text]
      , _category :: T.Text
      , _id :: T.Text
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
   = d
  | otherwise = d
parseQuizboxes bl = bl

-- parseMatching :: [Block] -> Quiz
-- parseMatching
parseHeader :: Quiz -> Block -> Quiz
parseHeader q (Header 2 (id_, cls, kvs) text) = set title text q
parseHeader q _ = q

parseQuizMeta :: Quiz -> Block -> Quiz
parseQuizMeta q (CodeBlock (id_, cls, kvs) code) =
  if "yaml" `elem` cls
    then decodeEither' (encodeUtf8 code)
    else q
-- renderMatching :: Quiz -> Block
