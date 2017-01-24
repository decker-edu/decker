{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveGeneric #-}

module Test
  ( Question(..)
  , Answer(..)
  , Choice(..)
  , OneAnswer(..)
  , Difficulty(..)
  , Exam(..)
  , StudentExam(..)
  , Templates
  , compileTesterTemplates
  , selectTemplate
  ) where

import Data.Aeson.TH
import Control.Exception
import Data.Yaml
import Data.Aeson.Types
import Data.Char
import Data.Maybe
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import Data.Typeable
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Embed
import Utilities
import Student

data Question = Question
  { qstTopicId :: T.Text
  , qstLectureId :: T.Text
  , qstTitle :: T.Text
  , qstPoints :: Int
  , qstQuestion :: T.Text
  , qstAnswer :: Answer
  , qstDifficulty :: Difficulty
  , qstComment :: T.Text
  , qstCurrentNumber :: Int
  , qstBasePath :: String
  } deriving (Eq, Show, Typeable, Generic)

data Choice = Choice
  { choiceTheAnswer :: T.Text
  , choiceCorrect :: Bool
  } deriving (Eq, Show, Typeable)

data OneAnswer = OneAnswer
  { oneDetail :: T.Text
  , oneCorrect :: T.Text
  } deriving (Eq, Show, Typeable)

data Answer
  = MultipleChoice { answChoices :: [Choice]}
  | FillText { answFillText :: T.Text
            ,  answCorrectWords :: [T.Text]}
  | FreeForm { answHeightInMm :: Int
            ,  answCorrectAnswer :: T.Text}
  | MultipleAnswers { answWidthInMm :: Int
                   ,  answAnswers :: [OneAnswer]}
  deriving (Eq, Show, Typeable)

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Show, Typeable)

data Exam = Exam
  { examTitle :: T.Text
  , examModule :: T.Text
  , examStudentInfoFile :: FilePath
  , examDateTime :: T.Text
  , examDurationInMinutes :: Int
  , examNumberOfQuestions :: Int
  , examTrack :: Int
  , examLectureIds :: [T.Text]
  , examExcludedTopicIds :: [T.Text]
  } deriving (Eq, Show, Typeable)

data StudentExam = StudentExam
  { stdexExam :: Exam
  , stdexStudent :: Student
  }

$(deriveJSON
    defaultOptions
    { fieldLabelModifier = drop 5
    }
    ''StudentExam)

$(deriveJSON
    defaultOptions
    { fieldLabelModifier = drop 6
    }
    ''Choice)

$(deriveJSON
    defaultOptions
    { fieldLabelModifier = drop 3
    }
    ''OneAnswer)

$(deriveJSON
    defaultOptions
    { fieldLabelModifier = drop 4
    }
    ''Answer)

questionOptions =
  defaultOptions
  { fieldLabelModifier = drop 3
  }

instance ToJSON Question where
  toJSON = genericToJSON questionOptions
  toEncoding = genericToEncoding questionOptions

instance FromJSON Question where
  parseJSON (Object q) =
    Question <$> q .: "TopicId" <*> q .: "LectureId" <*> q .: "Title" <*>
    q .: "Points" <*>
    q .: "Question" <*>
    q .: "Answer" <*>
    q .: "Difficulty" <*>
    q .: "Comment" <*>
    q .:? "CurrentNumber" .!= 0 <*>
    q .:? "BasePath" .!= "."
  parseJSON invalid = typeMismatch "Question" invalid

-- $(deriveJSON
--       defaultOptions
--       { fieldLabelModifier = drop 3
--       }
--       ''Question)
$(deriveJSON
    defaultOptions
    { fieldLabelModifier = drop 4
    }
    ''Exam)

$(deriveJSON defaultOptions ''Difficulty)

mcKey = typeOf $ MultipleChoice []

ftKey = typeOf $ FillText "" []

ffKey = typeOf $ FreeForm 0 ""

type Templates = [(TypeRep, M.Template)]

selectTemplate :: Templates -> Question -> M.Template
-- selectTemplate templates question = fromJust $ lookup (typeOf $ qstAnswer question) templates
selectTemplate templates question =
  case qstAnswer question of
    MultipleChoice _ ->
      compileMustacheTemplate $ fixMustacheMarkup testerMultipleChoiceTemplate
    MultipleAnswers _ _ ->
      compileMustacheTemplate $ fixMustacheMarkup testerMultipleAnswersTemplate
    FillText _ _ ->
      compileMustacheTemplate $ fixMustacheMarkup testerFillTextTemplate
    FreeForm _ _ ->
      compileMustacheTemplate $ fixMustacheMarkup testerFreeFormTemplate

compileTesterTemplates :: Templates
compileTesterTemplates =
  [ ( mcKey
    , compileMustacheTemplate $ fixMustacheMarkup testerMultipleChoiceTemplate)
  , (ftKey, compileMustacheTemplate $ fixMustacheMarkup testerFillTextTemplate)
  , (ffKey, compileMustacheTemplate $ fixMustacheMarkup testerFreeFormTemplate)
  ]

compileMustacheTemplate :: T.Text -> M.Template
compileMustacheTemplate string =
  case M.compileTemplate "" string of
    Left err -> throw $ MustacheException $ show err
    Right template -> template
