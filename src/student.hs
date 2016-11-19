{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

-- | Provides data types for the student information. Data is read from YAML files.
module Student
  (Student(..)
  ,Students(..))
  where

import Control.Exception
import Data.Yaml
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Hashable

data Student = Student
    { std_uid :: T.Text
    , std_department :: T.Text
    , std_displayName :: T.Text
    , std_employeeNumber :: T.Text
    , std_givenName :: T.Text
    , std_mail :: T.Text
    , std_sAMAccountName :: T.Text
    , std_sn :: T.Text
    , std_track :: Int
    } deriving (Eq,Show,Generic)

instance Hashable Student

data Students =
    Students (Map.HashMap T.Text Student)
    deriving (Eq,Show)

$(deriveJSON
      defaultOptions
      { fieldLabelModifier = drop 4
      }
      ''Student)

$(deriveJSON defaultOptions ''Students)