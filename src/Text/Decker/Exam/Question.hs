{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Question where

import Control.Exception
import Control.Lens hiding (Choice)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char (toLower)
import Data.List qualified as List
import Data.Typeable
import Data.Yaml qualified as Y
import GHC.Generics hiding (Meta)
import Relude
import System.Directory
import Text.Decker.Internal.Diagnostics (Diagnostic, warning)
import Text.Decker.Internal.Meta

data Choice = Choice
  { _choiceTheAnswer :: Text,
    _choiceCorrect :: Bool
  }
  deriving (Eq, Show, Typeable)

makeLenses ''Choice

data OneAnswer = OneAnswer
  { _oneDetail :: Text,
    _oneCorrect :: Text
  }
  deriving (Eq, Show, Typeable)

makeLenses ''OneAnswer

data Answer
  = MultipleChoice {_answChoices :: [Choice]}
  | FillText
      { _answFillText :: Text,
        _answCorrectWords :: [Text]
      }
  | FreeForm
      { _answHeightInMm :: Int,
        _answCorrectAnswer :: Text
      }
  | Numerical
      { _answCorrectNumber :: Int
      }
  | MultipleAnswers
      { _answWidthInMm :: Int,
        _answAnswers :: [OneAnswer]
      }
  deriving (Eq, Show, Typeable)

makeLenses ''Answer

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Show, Typeable)

$(deriveJSON defaultOptions ''Difficulty)

data Question = Question
  { _qstTopicId :: Text,
    _qstLectureId :: Text,
    _qstTitle :: Text,
    _qstPoints :: Int,
    _qstQuestion :: Text,
    _qstAnswer :: Answer,
    _qstDifficulty :: Difficulty,
    _qstComment :: Text,
    _qstShuffleAnswers :: Bool,
    _qstCurrentNumber :: Int,
    _qstFilePath :: String,
    _qstExam :: Bool
  }
  deriving (Eq, Show, Typeable, Generic)

makeLenses ''Question

$(deriveJSON defaultOptions {fieldLabelModifier = drop 7} ''Choice)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''OneAnswer)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Answer)

questionOptions = defaultOptions {fieldLabelModifier = drop 4}

instance ToJSON Question where
  toJSON = genericToJSON questionOptions
  toEncoding = genericToEncoding questionOptions

instance FromJSON Question where
  parseJSON (Object q) =
    Question
      <$> (q .: "TopicId")
      <*> (q .: "LectureId")
      <*> (q .: "Title")
      <*> (q .: "Points")
      <*> (q .: "Question")
      <*> (q .: "Answer")
      <*> (q .: "Difficulty")
      <*> (q .: "Comment")
      <*> (q .:? "ShuffleAnswers" .!= True)
      <*> (q .:? "CurrentNumber" .!= 0)
      <*> (q .:? "FilePath" .!= ".")
      <*> (q .:? "Exam" .!= True)
  parseJSON invalid = typeMismatch "Question" invalid

readQuestion :: FilePath -> IO Question
readQuestion file = do
  abs <- makeAbsolute file
  result <- liftIO $ Y.decodeFileEither file
  case result of
    Right question -> return (set qstFilePath abs question)
    Left exception ->
      throw
        $ YamlException
        $ "Error parsing question: "
        ++ file
        ++ ", "
        ++ show exception

-- | The top-level fields of a question file.
questionFields :: [Text]
questionFields =
  [ "TopicId",
    "LectureId",
    "Title",
    "Points",
    "Question",
    "Answer",
    "Difficulty",
    "Comment",
    "ShuffleAnswers",
    "CurrentNumber",
    "FilePath",
    "Exam"
  ]

-- | The fields of each answer type, keyed by its @tag@.
answerFields :: [(Text, [Text])]
answerFields =
  [ ("MultipleChoice", ["Choices"]),
    ("FillText", ["FillText", "CorrectWords"]),
    ("FreeForm", ["HeightInMm", "CorrectAnswer"]),
    ("Numerical", ["CorrectNumber"]),
    ("MultipleAnswers", ["WidthInMm", "Answers"])
  ]

-- | Reports fields of a question file that the parser silently ignores, e.g. a
-- misspelled @ShuffleAnswer@. Parse errors are left to 'readQuestion'.
questionDiagnostics :: FilePath -> IO [Diagnostic]
questionDiagnostics file = do
  result <- Y.decodeFileEither file
  return $ case result of
    Right (Object q) -> unknown "" questionFields q <> answer (KeyMap.lookup "Answer" q)
    _ -> []
  where
    answer (Just (Object a))
      | Just (String tag) <- KeyMap.lookup "tag" a,
        Just fields <- List.lookup tag answerFields =
          unknown "Answer." ("tag" : fields) a
    answer _ = []
    unknown prefix known object =
      [ warning "quest-unknown-field" file Nothing (message prefix known key)
        | key <- map Key.toText (KeyMap.keys object),
          key `notElem` known
      ]
    message prefix known key =
      "unknown field "
        <> prefix
        <> key
        <> " is ignored"
        <> maybe "" (\k -> " (did you mean " <> prefix <> k <> "?)") (closest known key)

-- | The known field closest to a misspelled one, if any is close enough.
closest :: [Text] -> Text -> Maybe Text
closest known key =
  fmap snd
    $ viaNonEmpty head
    $ sortOn fst
      [(d, k) | k <- known, let d = distance (lower k) (lower key), d <= 2]
  where
    lower = map toLower . toString

-- | Levenshtein distance.
distance :: String -> String -> Int
distance a b = fromMaybe 0 $ viaNonEmpty last $ foldl' step [0 .. length a] b
  where
    step prev@(p : ps) c = scanl compute (p + 1) (zip3 a prev ps)
      where
        compute left (x, diag, up) = min (up + 1) $ min (left + 1) (diag + if x == c then 0 else 1)
    step [] _ = []
