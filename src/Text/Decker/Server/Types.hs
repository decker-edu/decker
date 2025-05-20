{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Types where

import Control.Concurrent.STM (TChan)
import Control.Lens
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time
import Network.Wai (pathInfo)
import Network.WebSockets
import Relude
import Web.Scotty.Trans

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type Error = Text

instance ScottyError Text where
  stringError = toText
  showError = toLazy

data ServerState = ServerState
  { _clients :: [Client],
    _observed :: Set.Set FilePath
  }

makeLenses ''ServerState

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
  { channel :: TChan ActionMsg,
    serverState :: TVar ServerState
  }
  deriving (Eq)

type AppScottyM a = ScottyT Text (ReaderT Server IO) a

type AppActionM a = ActionT Text (ReaderT Server IO) a

requestPathText :: AppActionM Text
requestPathText = Text.intercalate "/" . pathInfo <$> request

requestPathString :: AppActionM String
requestPathString = toString . Text.intercalate "/" . pathInfo <$> request
