{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.ActionContext where

import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Development.Shake hiding (doesDirectoryExist, putError)
import Relude hiding (state)
import Text.Decker.Server.Server
import Control.Lens

data MutableActionState = MutableActionState
  { _devRun :: Bool,
    _externalStatus :: [(String, Bool)],
    _server :: IORef (Maybe Server),
    _watch :: IORef Bool,
    _publicResource :: Development.Shake.Resource
  }

makeLenses ''MutableActionState

data ActionContext = ActionContext
  { _state :: MutableActionState
  }
  deriving (Typeable)

makeLenses ''ActionContext

actionContextKey :: TypeRep
actionContextKey = typeOf (undefined :: ActionContext)

actionContext :: Action ActionContext
actionContext =
  fromMaybe (error "Error getting action context") <$> getShakeExtra
