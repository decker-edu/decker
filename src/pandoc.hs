-- | Generally useful functions on pansoc data structures. Some in the IO monad.
module Pandoc
       (isCacheableURI, Pandoc.readMetaData)
       where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.MultiMap as MM
import Data.Digest.Pure.MD5
import qualified Data.Yaml as Y
import Development.Shake
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI
import System.Directory
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.Walk
import Utilities
import Context
import Debug.Trace


type MetaData = M.Map FilePath Y.Value

readMetaData :: [FilePath] -> IO MetaData
readMetaData metaFiles =
  do canonMetaFiles <- mapM canonicalizePath metaFiles
     aDataList <- mapM decodeYaml canonMetaFiles
     let joined = M.map joinHorizontally $ MM.toMap $ MM.fromList aDataList
     return $
       M.mapWithKey (joinVertically joined)
                    joined
  where decodeYaml
          :: FilePath -> IO (FilePath,Y.Value)
        decodeYaml file =
          do result <- Y.decodeFileEither file
             case result of
               Right object@(Y.Object _) -> return (takeDirectory file,object)
               Right _ ->
                 throw $
                 YamlException $
                 "Top-level meta value must be an object: " ++ file
               Left exception -> throw exception
        joinHorizontally :: [Y.Value] -> Y.Value
        joinHorizontally = foldl1 joinMeta
        joinVertically
          :: MetaData -> FilePath -> Y.Value -> Y.Value
        joinVertically meta dir ignore =
          if not $ equalFilePath dir "/"
             then let up =
                        joinVertically meta
                                       (takeDirectory dir)
                                       ignore
                  in maybe up
                           (joinMeta up)
                           (M.lookup dir meta)
             else Y.Object (H.fromList [])
        joinMeta :: Y.Value -> Y.Value -> Y.Value
        joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
        joinMeta _ _ = throw $ YamlException "Can only join YAML objects."

-- extractMetaDataFromMarkdown :: T.Text -> Y.Value 
