{-# LANGUAGE DeriveDataTypeable #-}

module Context
       (ActionContext(..), makeActionContext, setActionContext, getFilesToWatch,
        setFilesToWatch, getServerHandle, setServerHandle, getProjectDir,
        getPublicDir, getCacheDir, actionContextKey, getActionContext)
       where

import Control.Monad()
import Development.Shake
import Data.Dynamic
import Data.Maybe
import Data.IORef
import Data.Typeable()
import qualified Data.HashMap.Lazy as HashMap
import System.Process
import Text.Printf

data ActionContext =
  ActionContext {ctxFilesToWatch :: IORef [FilePath]
                ,ctxServerHandle :: IORef (Maybe ProcessHandle)
                ,ctxProjectDir :: FilePath
                ,ctxPublicDir :: FilePath
                ,ctxCacheDir :: FilePath}
  deriving (Typeable)

instance Show ActionContext where
  show ctx =
    printf "ActionContext {ctxProjectDir = %s, ctxPublicDir = %s, ctxCacheDir = %s}"
           (ctxProjectDir ctx)
           (ctxPublicDir ctx)
           (ctxCacheDir ctx)

defaultActionContext :: IO ActionContext
defaultActionContext = do
  files <- newIORef []
  server <- newIORef Nothing
  return $ ActionContext files server "" "" ""

actionContextKey :: IO TypeRep
actionContextKey = do
  ctx <- liftIO $ defaultActionContext
  return $ typeOf ctx

makeActionContext :: FilePath -> FilePath -> FilePath -> IO ActionContext
makeActionContext projectDir publicDir cacheDir =
  do ctx <- defaultActionContext
     return $
       ctx {ctxProjectDir = projectDir
           ,ctxPublicDir = publicDir
           ,ctxCacheDir = cacheDir}

setActionContext :: ActionContext -> ShakeOptions -> IO ShakeOptions
setActionContext ctx options =
  do key <- liftIO $ actionContextKey
     let extra = HashMap.insert key (toDyn ctx) $ HashMap.empty
     return options {shakeExtra = extra}

getActionContext :: Action ActionContext
getActionContext = do
  options <- getShakeOptions
  key <- liftIO $ actionContextKey
  let extra = shakeExtra options
  let dyn = case HashMap.lookup key extra of
              Just d -> d
              Nothing -> error "Error looking up action context"
  return $ case fromDynamic dyn of
             Just d -> d
             Nothing -> error "Error upcasting action context"

getFilesToWatch :: Action [FilePath]
getFilesToWatch = do
  ctx <- getActionContext
  liftIO $ readIORef $ ctxFilesToWatch ctx

setFilesToWatch :: [FilePath] -> Action ()
setFilesToWatch files = do
  ctx <- getActionContext
  liftIO $ writeIORef (ctxFilesToWatch ctx) files

getServerHandle :: Action (Maybe ProcessHandle)
getServerHandle = do
  ctx <- getActionContext
  liftIO $ readIORef $ ctxServerHandle ctx

setServerHandle :: Maybe ProcessHandle -> Action ()
setServerHandle handle = do
  ctx <- getActionContext
  liftIO $ writeIORef (ctxServerHandle ctx) handle

getProjectDir :: Action FilePath
getProjectDir =
  do ctx <- getActionContext
     return $ ctxProjectDir ctx

getPublicDir :: Action FilePath
getPublicDir =
  do ctx <- getActionContext
     return $ ctxPublicDir ctx

getCacheDir :: Action FilePath
getCacheDir =
  do ctx <- getActionContext
     return $ ctxCacheDir ctx
