{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Action
  ( spawn
  , wantRepeat
  , dropSuffix
  , runHttpServer
  , openBrowser
  , replaceSuffix
  , replaceSuffixWith
  , globA
  , reloadBrowsers
  , calcTargets
  , calcSource
  , readMetaDataForDir
  , DeckerException(..)
  ) where
 
import Common
import Exception
import Context
import Control.Exception
import Data.IORef
import Data.List as List
import Data.List (isInfixOf)
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Yaml as Y
import Development.Shake
import Development.Shake.FilePath as SFP
import Meta
import Project
import Server
import System.FilePath.Glob
import System.Info (os)
import System.Process

-- | Globs for files under the project dir in the Action monad. Returns absolute
-- pathes.
globA :: FilePattern -> Action [FilePath]
globA pat = do
  dirs <- getProjectDirs
  liftIO $
    (filter (not . isPrefixOf (public dirs)) . sort) <$>
    globDir1 (compile pat) (project dirs)

-- Utility functions for shake based apps
spawn :: String -> Action ProcessHandle
spawn = liftIO . spawnCommand

-- Runs the built-in server on the given directory, if it is not already
-- running.
runHttpServer :: Int -> ProjectDirs -> Maybe String -> Action ()
runHttpServer port dirs url = do
  server <- getServerHandle
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- liftIO $ startHttpServer dirs port
      setServerHandle $ Just httpServer
      case url of
        Just u -> openBrowser u
        Nothing -> return ()

openBrowser :: String -> Action ()
openBrowser url = do
  if | any (`isInfixOf` os) ["linux", "bsd"] ->
       liftIO $ callProcess "xdg-open" [url]
     | "darwin" `isInfixOf` os -> liftIO $ callProcess "open" [url]
     | otherwise ->
       putNormal $ "Unable to open browser on this platform for url: " ++ url

reloadBrowsers :: Action ()
reloadBrowsers = do
  server <- getServerHandle
  case server of
    Just serv -> liftIO $ reloadClients serv
    Nothing -> return ()

wantRepeat :: IORef Bool -> Action ()
wantRepeat justOnce = liftIO $ writeIORef justOnce False

-- | Calculates the target pathes from a list of source files.
calcTargets :: String -> String -> [FilePath] -> Action [FilePath]
calcTargets srcSuffix targetSuffix sources = do
  dirs <- getProjectDirs
  return $
    map
      (replaceSuffix srcSuffix targetSuffix .
       combine (public dirs) . makeRelative (project dirs))
      sources

-- | Calculate the source file from the target path. Calls need.
calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target = do
  dirs <- getProjectDirs
  let src =
        (replaceSuffix targetSuffix srcSuffix .
         combine (project dirs) . makeRelative (public dirs))
          target
  need [src]
  return src

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

-- | Monadic version of suffix replacement for easy binding.
replaceSuffixWith :: String -> String -> [FilePath] -> Action [FilePath]
replaceSuffixWith suffix with pathes =
  return [dropSuffix suffix d ++ with | d <- pathes]

readMetaDataForDir :: FilePath -> Action Y.Value
readMetaDataForDir directory = walkUpTo directory
  where
    walkUpTo dir = do
      dirs <- getProjectDirs
      if equalFilePath (project dirs) dir
        then collectMeta dir
        else do
          fromAbove <- walkUpTo (takeDirectory dir)
          fromHere <- collectMeta dir
          return $ joinMeta fromHere fromAbove
    --
    collectMeta dir = do
      files <- liftIO $ globDir1 (compile "*-meta.yaml") dir
      need files
      meta <- mapM decodeYaml files
      return $ foldl joinMeta (Y.object []) meta
    --
    decodeYaml yamlFile = do
      result <- liftIO $ Y.decodeFileEither yamlFile
      case result of
        Right object@(Y.Object _) -> return object
        Right _ ->
          throw $
          YamlException $
          "Top-level meta value must be an object: " ++ directory
        Left exception -> throw exception
