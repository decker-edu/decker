{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Types where

import Control.Concurrent
import Control.Lens
import qualified Data.Set as Set
import Network.WebSockets
import Relude

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type ServerState = ([Client], Set.Set FilePath)

data ActionMsg
  = PortInUse Int
  | FileChanged FilePath
  deriving (Show, Eq)

data Server = Server
  { _threadId :: ThreadId,
    _serverState :: MVar ServerState
  }
  deriving (Eq)

makeLenses ''Server
