{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attributes where

import Control.Lens
import qualified Data.Set as Set
import Relude hiding (id)
import Text.Decker.Filter.Monad

type Id = Text

type Classes = Set Text

type Attribs = Map Text Text

data ElemParams = ElemParams
  { _id :: Text,
    _cs :: Set Text,
    _kvs :: Map Text Text,
    _qts :: Set Text,
    _qps :: Map Text Text
  }
  deriving (Show)

makeLenses ''ElemParams

data AttribState = AttribState
  { _src :: ElemParams,
    _dst :: ElemParams
  }
  deriving (Show)

makeLenses ''AttribState

class DstOp a where
  set :: a -> Attrib ()
  add :: a -> Attrib ()

class SrcOp a where
  get :: a -> Attrib a
  take :: a -> Attrib a
  drop :: a -> Attrib ()

instance DstOp Text where
  set = setId
  add = setId

instance SrcOp Text where
  get _ = getId
  take _ = takeId
  drop _ = dropId

instance DstOp Classes where
  set = setCls
  add = addCls

instance SrcOp Classes where
  get = getCls
  take = takeCls
  drop = dropCls

instance DstOp [Text] where
  set = setCls'
  add = addCls'

instance SrcOp [Text] where
  get = getCls'
  take = takeCls'
  drop = dropCls'

getId :: Attrib Id
getId = use (src . id)

takeId :: Attrib Id
takeId = do
  i <- getId
  assign (src . id) ""
  return i

dropId :: Attrib ()
dropId = assign (src . id) ""

setId :: Id -> Attrib ()
setId i = assign (src . id) i

getCls :: Classes -> Attrib Classes
getCls want | Set.null want = use (src . cs)
getCls want = do
  cls <- use (src . cs)
  return (Set.intersection cls want)

getCls' :: [Text] -> Attrib [Text]
getCls' want = toList <$> getCls (fromList want)

takeCls :: Classes -> Attrib Classes
takeCls want | Set.null want = do
  cls <- use (src . cs)
  assign (src . cs) Set.empty
  return cls
takeCls want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)
  return (Set.intersection cls want)

takeCls' :: [Text] -> Attrib [Text]
takeCls' want = toList <$> takeCls (fromList want)

dropCls :: Classes -> Attrib ()
dropCls want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)

dropCls' :: [Text] -> Attrib ()
dropCls' want = dropCls (fromList want)

setCls :: Classes -> Attrib ()
setCls cls = assign (dst . cs) cls

setCls' :: [Text] -> Attrib ()
setCls' = setCls . fromList

addCls :: Classes -> Attrib ()
addCls new = do
  cls <- use (src . cs)
  assign (dst . cs) (Set.union cls new)

addCls' :: [Text] -> Attrib ()
addCls' = addCls . fromList

type Attrib = StateT AttribState Filter

{-
 -getAttribute :: Text -> Attrib (Maybe (Text, Text))
 -getAttribute key = undefined
 -
 -setAttribute :: (Maybe (Text, Text)) -> Attrib ()
 -setAttribute key value = undefined
 -}
