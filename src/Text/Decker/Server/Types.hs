{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Types where

import Control.Concurrent
import Control.Lens
import qualified Data.Set as Set
import Data.Time
import Network.WebSockets
import Relude
import Web.Scotty.Trans

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type Error = Text

instance ScottyError Text where
  stringError = toText
  showError = toLazy

type AppScottyM = ScottyT Text ServerM

type AppActionM = ActionT Text ServerM

newtype ServerM a = ServerM
  { runServerM :: ReaderT Server IO a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadReader Server
    )

data ServerState = ServerState
  { clients :: [Client],
    observed :: Set.Set FilePath
  }

data VideoOperation
  = Replace FilePath FilePath
  | Append FilePath FilePath
  deriving (Show)

data ActionMsg
  = ServerExit String
  | FileChanged UTCTime FilePath
  | UploadComplete VideoOperation
  deriving (Show)

data Server = Server
  { _threadId :: ThreadId,
    _serverState :: TVar ServerState
  }
  deriving (Eq)

makeLenses ''Server
