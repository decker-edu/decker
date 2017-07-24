{-- Author: Henrik Tramberend <henrik@tramberend.de> --} 

{-# LANGUAGE DeriveDataTypeable #-}

module Context
       (ActionContext(..), makeActionContext, setActionContext, getFilesToWatch,
        setFilesToWatch, getServerHandle, setServerHandle, getProjectDirs,
        actionContextKey, getActionContext)
       where

import Control.Monad ()
import Development.Shake
import Data.Dynamic
import Data.Maybe ()
import Data.IORef
import Data.Typeable ()
import qualified Data.HashMap.Lazy as HashMap
import System.Process
import Project

data ActionContext =
  ActionContext {ctxFilesToWatch :: IORef [FilePath]
                ,ctxServerHandle :: IORef (Maybe ProcessHandle)
                ,ctxDirs :: ProjectDirs}
  deriving (Typeable, Show)

instance Show (IORef a) where
  show _ = "IORef"

defaultActionContext :: IO ActionContext
defaultActionContext = do
  files <- newIORef []
  server <- newIORef Nothing
  return $ ActionContext files server (ProjectDirs "" "" "" "")

actionContextKey :: IO TypeRep
actionContextKey = do
  ctx <- liftIO $ defaultActionContext
  return $ typeOf ctx

makeActionContext :: ProjectDirs -> IO ActionContext
makeActionContext dirs =
  do ctx <- defaultActionContext
     return $
       ctx { ctxDirs = dirs }

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

getProjectDirs :: Action ProjectDirs
getProjectDirs =
  do ctx <- getActionContext
     return $ ctxDirs ctx

