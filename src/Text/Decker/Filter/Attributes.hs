{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attributes where

import Text.Decker.Filter.Monad

import Control.Lens
import Relude hiding (id)

data ElemParams = ElemParams
  { _id :: Text
  , _cs :: Set Text
  , _kvs :: Map Text Text
  , _qts :: Set Text
  , _qps :: Map Text Text
  } deriving (Show)

makeLenses ''ElemParams

data AttribState = AttribState
  { _src :: ElemParams
  , _dst :: ElemParams
  } deriving (Show)

makeLenses ''AttribState

srcId :: Attrib Text
srcId = do
  i <- use (src . id)
  assign (src . id) ""
  return i

dstId :: Text -> Attrib ()
dstId = assign (src . id)

type Attrib = StateT AttribState Filter
{-
 -getAttribute :: Text -> Attrib (Maybe (Text, Text))
 -getAttribute key = undefined
 -
 -setAttribute :: (Maybe (Text, Text)) -> Attrib ()
 -setAttribute key value = undefined
 -}
