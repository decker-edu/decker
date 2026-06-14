{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Exam.Exam
  ( Exam (..),
    ExamTopic (..),
    examTitle,
    examTopics,
    etLectureId,
    etTopicId,
    readExam,
    matchesExam,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson.Types
import Data.Set qualified as Set
import Data.Yaml qualified as Y
import Relude
import Text.Decker.Exam.Question
import Text.Decker.Internal.Exception

data ExamTopic = ExamTopic
  { _etLectureId :: Text,
    _etTopicId :: Text
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''ExamTopic

instance FromJSON ExamTopic where
  parseJSON (Object o) =
    ExamTopic <$> (o .: "LectureId") <*> (o .: "TopicId")
  parseJSON invalid = typeMismatch "ExamTopic" invalid

data Exam = Exam
  { _examTitle :: Text,
    _examTopics :: [ExamTopic]
  }
  deriving (Eq, Show, Generic)

makeLenses ''Exam

instance FromJSON Exam where
  parseJSON (Object o) =
    Exam <$> (o .: "Title") <*> (o .: "Topics")
  parseJSON invalid = typeMismatch "Exam" invalid

readExam :: FilePath -> IO Exam
readExam file = do
  result <- Y.decodeFileEither file
  case result of
    Right exam -> return exam
    Left ex ->
      throw
        $ YamlException
        $ "Error parsing exam: "
        ++ file
        ++ ", "
        ++ show ex

-- | Question selector for an exam: question must be flagged for export
-- and its (LectureId, TopicId) must appear in the exam's topic list.
matchesExam :: Exam -> Question -> Bool
matchesExam exam q =
  _qstExam q
    && Set.member (_qstLectureId q, _qstTopicId q) topicSet
  where
    topicSet =
      Set.fromList
        $ map (\t -> (_etLectureId t, _etTopicId t)) (_examTopics exam)
