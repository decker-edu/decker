{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Context
  ( ActionContext(..)
  , makeActionContext
  , setActionContext
  , getFilesToWatch
  , setFilesToWatch
  , getServerHandle
  , setServerHandle
  , getProjectDirs
  , actionContextKey
  , getActionContext
  , getPublicResource
  , withShakeLock
  , getRelativeSupportDir
  ) where

import Common
import Exception
import Control.Monad ()
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Typeable (TypeRep, typeOf)
import Development.Shake as Shake
import Project
import Server
import System.FilePath

data ActionContext = ActionContext
  { ctxFilesToWatch :: IORef [FilePath]
  , ctxServerHandle :: IORef (Maybe Server)
  , ctxDirs :: ProjectDirs
  , ctxPublicResource :: Shake.Resource
  } deriving (Typeable, Show)

instance Show (IORef a) where
  show _ = "IORef"

defaultActionContext :: IO ActionContext
defaultActionContext = do
  files <- newIORef []
  server <- newIORef Nothing
  resource <- newResourceIO "PublicDir" 1
  return $ ActionContext files server (ProjectDirs "" "" "" "" "" "") resource

actionContextKey :: IO TypeRep
actionContextKey = do
  ctx <- liftIO defaultActionContext
  return $ typeOf ctx

makeActionContext :: ProjectDirs -> IO ActionContext
makeActionContext dirs = do
  ctx <- defaultActionContext
  return $ ctx {ctxDirs = dirs}

setActionContext :: ActionContext -> ShakeOptions -> IO ShakeOptions
setActionContext ctx options = do
  key <- liftIO actionContextKey
  let extra = HashMap.insert key (toDyn ctx) HashMap.empty
  return options {shakeExtra = extra}

getActionContext :: Action ActionContext
getActionContext = do
  options <- getShakeOptions
  key <- liftIO actionContextKey
  let extra = shakeExtra options
  let dyn =
        fromMaybe
          (error "Error looking up action context")
          (HashMap.lookup key extra)
  return $ fromMaybe (error "Error upcasting action context") (fromDynamic dyn)

getFilesToWatch :: Action [FilePath]
getFilesToWatch = do
  ctx <- getActionContext
  liftIO $ readIORef $ ctxFilesToWatch ctx

setFilesToWatch :: [FilePath] -> Action ()
setFilesToWatch files = do
  ctx <- getActionContext
  liftIO $ writeIORef (ctxFilesToWatch ctx) files

getServerHandle :: Action (Maybe Server)
getServerHandle = do
  ctx <- getActionContext
  liftIO $ readIORef $ ctxServerHandle ctx

setServerHandle :: Maybe Server -> Action ()
setServerHandle handle = do
  ctx <- getActionContext
  liftIO $ writeIORef (ctxServerHandle ctx) handle

getProjectDirs :: Action ProjectDirs
getProjectDirs = do
  ctx <- getActionContext
  return $ ctxDirs ctx

getPublicResource :: Action Shake.Resource
getPublicResource = do
  ctx <- getActionContext
  return $ ctxPublicResource ctx

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  publicResource <- getPublicResource
  withResource publicResource 1 perform

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from = do
  pub <- public <$> getProjectDirs
  let sup = pub </> ("support" ++ "-" ++ deckerVersion)
  return $ makeRelativeTo from sup
