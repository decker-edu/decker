{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( DeckerException(..)
  ) where

import Control.Exception
import Data.Typeable

-- | Tool specific exceptions
data DeckerException
  = MustacheException String
  | ResourceException String
  | GitException String
  | PandocException String
  | YamlException String
  | HttpException String
  | RsyncUrlException
  | DecktapeException String
  deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (ResourceException e) = e
  show (GitException e) = e
  show (HttpException e) = e
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
