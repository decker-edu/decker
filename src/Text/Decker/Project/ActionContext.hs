{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.ActionContext where

import Control.Concurrent.STM
import Control.Lens
import Data.Dynamic
import Data.Maybe
import Data.Typeable
import Development.Shake hiding (doesDirectoryExist, putError)
import Relude hiding (state)
import Text.Decker.Server.Types
import Text.Pandoc

data Flags
  = MetaValueFlag
      String
      String
  | FormatFlag
  | CleanFlag
  | CleanerFlag
  | WatchFlag
  | NoRebuildFlag
  | ServerFlag
  | ErrorFlag
  | ThreadFlag
  | OpenFlag
  | PoserFlag
  | PortFlag Int
  | BindFlag String
  deriving (Eq, Show)

data ActionContext = ActionContext
  { _extra :: [Flags],
    _devRun :: Bool,
    _externalStatus :: [(String, Bool)],
    _server :: TVar ServerState,
    _watch :: IORef Bool,
    _actionChan :: TChan ActionMsg,
    _publicResource :: Development.Shake.Resource,
    _globalMeta :: Meta
  }
  deriving (Typeable)

makeLenses ''ActionContext

actionContextKey :: TypeRep
actionContextKey = typeRep (Proxy :: Proxy ActionContext)

actionContext :: Action ActionContext
actionContext =
  fromMaybe (error "Error getting action context") <$> getShakeExtra
