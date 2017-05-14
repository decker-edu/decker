{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

-- | Provides data types for the student information. Data is read from YAML files.
module Student
  ( Student(..)
  , Students(..)
  ) where

import Control.Exception
import Data.Aeson.TH
import Data.Aeson.Types
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics (Generic)

data Student = Student
  { std_displayName :: T.Text
  , std_employeeNumber :: T.Text
  , std_givenName :: T.Text
  , std_mail :: T.Text
  , std_sAMAccountName :: T.Text
  , std_sn :: T.Text
  , std_track :: Int
  } deriving (Eq, Show, Generic)

instance Hashable Student

data Students = Students
  { stds_course :: T.Text
  , stds_semester :: T.Text
  , stds_students :: Map.HashMap T.Text Student
  } deriving (Eq, Show)

-- Students (Map.HashMap T.Text Student)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Student)

$(deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Students)
