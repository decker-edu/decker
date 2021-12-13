{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attributes where

import Control.Lens
import qualified Data.Set as Set
import qualified Data.Map as Map
import Relude hiding (id)
import Text.Decker.Filter.Monad

type Id = Text

type Classes = Set Text

type Key = Text

type Value = Text

type Attribs = Map Key Value

data ElemParams = ElemParams
  { _id :: Text,
    _cs :: Classes,
    _kvs :: Attribs,
    _qts :: Classes,
    _qps :: Attribs
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
  set = setCls'
  add = addCls'

instance SrcOp Classes where
  get = getCls'
  take = takeCls'
  drop = dropCls'

instance DstOp [Text] where
  set = setCls
  add = addCls

instance SrcOp [Text] where
  get = getCls
  take = takeCls
  drop = dropCls

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

getCls' :: Classes -> Attrib Classes
getCls' want | Set.null want = use (src . cs)
getCls' want = do
  cls <- use (src . cs)
  return (Set.intersection cls want)

getCls :: [Text] -> Attrib [Text]
getCls want = toList <$> getCls' (fromList want)

takeCls' :: Classes -> Attrib Classes
takeCls' want | Set.null want = do
  cls <- use (src . cs)
  assign (src . cs) Set.empty
  return cls
takeCls' want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)
  return (Set.intersection cls want)

takeCls :: [Text] -> Attrib [Text]
takeCls want = toList <$> takeCls' (fromList want)

dropCls' :: Classes -> Attrib ()
dropCls' want | Set.null want = do
  assign (src . cs) Set.empty
dropCls' want = do
  cls <- use (src . cs)
  assign (src . cs) (Set.difference cls want)

dropCls :: [Text] -> Attrib ()
dropCls want = dropCls' (fromList want)

setCls' :: Classes -> Attrib ()
setCls' cls = assign (dst . cs) cls

setCls :: [Text] -> Attrib ()
setCls = setCls' . fromList

addCls' :: Classes -> Attrib ()
addCls' new = do
  cls <- use (src . cs)
  assign (dst . cs) (Set.union cls new)

addCls :: [Text] -> Attrib ()
addCls = addCls' . fromList

getAttr :: [Key] -> Attrib Attribs
getAttr want | null want = use (src . kvs)
getAttr want = do
  attr <- use (src . kvs)
  return (Map.filterWithKey (\k _ -> k `elem` want) attr)

takeAttr :: [Key] -> Attrib Attribs
takeAttr want | null want = do
  attr <- use (src . kvs)
  assign (src . kvs) Map.empty
  return attr
takeAttr want = do
  attr <- use (src . kvs)
  assign (src . kvs) (Map.filterWithKey (\k _ -> k `notElem` want) attr)
  return (Map.filterWithKey (\k _ -> k `elem` want) attr)

dropAttr :: [Key] -> Attrib ()
dropAttr want | null want = do
  assign (src . kvs) Map.empty
dropAttr want = do
  attr <- use (src . kvs)
  assign (src . kvs) (Map.filterWithKey (\k _ -> k `notElem` want) attr)

type Attrib = StateT AttribState Filter

-- Some tests

