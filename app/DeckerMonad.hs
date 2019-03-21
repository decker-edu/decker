module DeckerMonad
  (
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Text as Text
import qualified Development.Shake as Shake

-- | The decker monad all processing takes place here. It defines the oberall
-- structure of the program
newtype DeckerMonad a = DeckerMonad
  { runDecker :: ReaderT DeckerData (StateT DeckerState (ExceptT DeckerError Shake.Action)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader DeckerData
             , MonadState DeckerState
             , MonadError DeckerError
             )

data DeckerData = DeckerData
  {
  } deriving (Show, Eq)

data DeckerState = DeckerState
  {
  } deriving (Show, Eq)

-- | All the reasons why a decker operation might fail.
data DeckerError
  = UnknownError { description :: Text.Text }
  | DeckerIOError { description :: Text.Text }
  | ResourceNotFound { resource :: FilePath
                     , referrer :: Maybe FilePath }
