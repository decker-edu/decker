{-- Author: Henrik Tramberend <henrik@tramberend.de> --} 

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Assignment
  (
  ) where

import Control.Lens
import Data.Time
import Data.Yaml
import qualified Data.HashMap.Strict as M
import GHC.Generics

data DueDate = DueDate
  { _date :: UTCTime
  , _track :: Int
  } deriving (Show, Generic)

instance FromJSON DueDate

data Assignment = Assignment
  { _tag :: String
  , _artefacts :: [String]
  , _due :: [DueDate]
  } deriving (Show, Generic)

instance FromJSON Assignment

type Assignments = M.HashMap String Assignment

data AssignmentInfo = AssignmentInfo
  { _graceperiod :: Int
  , _assignments :: Assignments
  } deriving (Show, Generic)
