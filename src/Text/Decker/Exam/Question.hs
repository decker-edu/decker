{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Question where

import Control.Exception
import Control.Lens hiding (Choice)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Typeable
import qualified Data.Yaml as Y
import GHC.Generics hiding (Meta)
import Relude
import System.Directory
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
    _qstFilePath :: String
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
    Question <$> q .: "TopicId" <*> q .: "LectureId" <*> q .: "Title"
      <*> q .: "Points"
      <*> q .: "Question"
      <*> q .: "Answer"
      <*> q .: "Difficulty"
      <*> q .: "Comment"
      <*> q .:? "ShuffleAnswers" .!= True
      <*> q .:? "CurrentNumber" .!= 0
      <*> q .:? "FilePath" .!= "."
  parseJSON invalid = typeMismatch "Question" invalid

readQuestion :: FilePath -> IO Question
readQuestion file = do
  abs <- makeAbsolute file
  result <- liftIO $ Y.decodeFileEither file
  case result of
    Right question -> return (set qstFilePath abs question)
    Left exception ->
      throw $
        YamlException $
          "Error parsing question: " ++ file ++ ", " ++ show exception
