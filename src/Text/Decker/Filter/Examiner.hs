{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings,
  DeriveGeneric, DeriveFunctor, DeriveTraversable #-}

module Text.Decker.Filter.Examiner
  ( Question(..)
  , Answer(..)
  , Choice(..)
  , OneAnswer(..)
  , Difficulty(..)
  ) where

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Typeable
import GHC.Generics
import Relude

data Question a = Question
  { qstTopicId :: Text
  , qstLectureId :: Text
  , qstTitle :: a
  , qstPoints :: Int
  , qstQuestion :: a
  , qstAnswer :: Answer a
  , qstDifficulty :: Difficulty
  , qstComment :: a
  , qstCurrentNumber :: Int
  , qstFilePath :: String
  } deriving (Eq, Show, Typeable, Generic, Functor, Foldable, Traversable)

data Choice a = Choice
  { choiceTheAnswer :: a
  , choiceCorrect :: Bool
  } deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

data OneAnswer a = OneAnswer
  { oneDetail :: a
  , oneCorrect :: a
  } deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

data Answer a
  = MultipleChoice { answChoices :: [Choice a] }
  | FillText { answFillText :: a
             , answCorrectWords :: [a] }
  | FreeForm { answHeightInMm :: Int
             , answCorrectAnswer :: a }
  | MultipleAnswers { answWidthInMm :: Int
                    , answAnswers :: [OneAnswer a] }
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Show, Typeable)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''Choice)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''OneAnswer)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Answer)

questionOptions = defaultOptions {fieldLabelModifier = drop 3}

instance ToJSON a => ToJSON (Question a) where
  toJSON = genericToJSON questionOptions
  toEncoding = genericToEncoding questionOptions

instance FromJSON a => FromJSON (Question a) where
  parseJSON (Object q) =
    Question <$> q .: "TopicId" <*> q .: "LectureId" <*> q .: "Title" <*>
    q .: "Points" <*>
    q .: "Question" <*>
    q .: "Answer" <*>
    q .: "Difficulty" <*>
    q .: "Comment" <*>
    q .:? "CurrentNumber" .!= 0 <*>
    q .:? "FilePath" .!= "."
  parseJSON invalid = typeMismatch "Question" invalid

-- $(deriveJSON
--       defaultOptions
--       { fieldLabelModifier = drop 3
--       }
--       ''Question)
$(deriveJSON defaultOptions ''Difficulty)

--readQuestion :: FilePath -> IO Question
--readQuestion path = undefined
