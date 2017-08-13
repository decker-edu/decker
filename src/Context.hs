{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE DeriveDataTypeable #-}

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
  ) where

import Control.Monad ()
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Typeable ()
import Development.Shake
import Project
import System.Process
import Server

data ActionContext = ActionContext
  { ctxFilesToWatch :: IORef [FilePath]
  , ctxServerHandle :: IORef (Maybe Server)
  , ctxDirs :: ProjectDirs
  } deriving (Typeable, Show)

instance Show (IORef a) where
  show _ = "IORef"

defaultActionContext :: IO ActionContext
defaultActionContext = do
  files <- newIORef []
  server <- newIORef Nothing
  return $ ActionContext files server (ProjectDirs "" "" "" "" "")

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
