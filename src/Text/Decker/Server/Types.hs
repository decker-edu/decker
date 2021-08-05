module Text.Decker.Server.Types where

import Control.Concurrent
import qualified Data.Set as Set
import Network.WebSockets

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type ServerState = ([Client], Set.Set FilePath)

type Server = (ThreadId, MVar ServerState)
