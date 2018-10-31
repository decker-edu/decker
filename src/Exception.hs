module Exception
  ( DeckerException(..)
  ) where

import Control.Exception
import Data.Typeable

-- | Tool specific exceptions
data DeckerException
  = MustacheException String
  | InternalException String
  | ResourceException String
  | GitException String
  | PandocException String
  | YamlException String
  | HttpException String
  | RsyncUrlException
  | DecktapeException String
  | ExternalException String
  | SassException String
  deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
  show (InternalException e) = e
  show (MustacheException e) = e
  show (ResourceException e) = e
  show (GitException e) = e
  show (HttpException e) = e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
  show (ExternalException e) = e
  show (SassException e) = e
