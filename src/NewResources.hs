{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  ( writeExampleProject
  , copyDir
  , copyResource
  , linkResource
  ) where

import Common
import Project

import Control.Exception
import Control.Monad.Extra
import qualified System.Directory as D
import System.FilePath

{- NOTE:
Plan:
First move all resource-related functions to this module
then refactor and design new interface

resources is already in meta data (read from decker-meta.yaml)
see metaA in Shake.hs
this metadata is currently only read in Decker.hs and then propagated forward
This should stay. Here in NewResources only the ResourceType Datatype should be used.
Paths come from Project.hs
What does "provision" actually mean in this context?

TODO: From Decker.hs
move functionality from "support" command to here
    checks if support directory exists and checks provisioning type
    symlinks/copies accordingly

TODO: from Resource.hs
getOldResources 
    (maybe to Project bc it returns Paths?)
    Returns IO [FilePath] containing the paths of all old, cached resource folders
getResourceString (not sure if this fits here. 
    outputs a file as string. e.g. the help-page
    maybe add an entire "Output.hs" module?)
extractResources
    Extract resources from the executable into the XDG data directory
    Currently using cli command unzip
unzip
    calls "unzip"
writeResourceFiles
-}
-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = writeResourceFiles "example" "."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  copyDir src destDir

-- After moving functionality of "decker support" (in Decker.hs) to NewResources module
-- hide this function (this is pure utility)
-- | Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (D.doesDirectoryExist src) $
    throw (userError "src does not exist or is not a directory")
  dstExists <- D.doesDirectoryExist dst
  if dstExists && (last (splitPath src) /= last (splitPath dst))
    then copyDir src (dst </> last (splitPath src))
    else do
      D.createDirectoryIfMissing True dst
      contents <- D.listDirectory src
      forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- D.doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else copyFileIfNewer srcPath dstPath

{-
| TODO: From Project.hs
copyFileIfNewer - check
fileIsNewer - check
copyResource - check
linkResource - check
what about
absRefResource, relRefResource?
-}
-- | Copies the src to dst if src is newer or dst does not exist. Creates
-- missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst =
  whenM (fileIsNewer src dst) $ do
    D.createDirectoryIfMissing True (takeDirectory dst)
    D.copyFile src dst

fileIsNewer :: FilePath -> FilePath -> IO Bool
fileIsNewer a b = do
  aexists <- D.doesFileExist a
  bexists <- D.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- D.getModificationTime a
             bt <- D.getModificationTime b
             return (at > bt)
           else return False
    else return aexists

{-
TODO: These two functions (together with provisionResource and its called functions
from Utilities.hs) should maybe be pulled apart?
Not sure how I feel about a function returning a FilePath in this module
-}
-- | Copies single Resource file and returns Filepath
copyResource :: Resource -> IO FilePath
copyResource resource = do
  copyFileIfNewer (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

-- | Creates SymLink to single resource file and returns Filepath
linkResource :: Resource -> IO FilePath
linkResource resource = do
  whenM
    (D.doesFileExist (publicFile resource))
    (D.removeFile (publicFile resource))
  D.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  D.createFileLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)
{-
| TODO: From Utilities.hs
getSupportDir (move to Project.hs b/c it returns a path)
provisionResources
    step in the pipeline of readAndProcessMarkdown
    type Decker = StateT DeckerState Action
    state transformer 
provisionMetaResource
    what does this do?
    returns an Action FilePath

provisionTemplateOverrideSupport
provisionTemplateOverrideSupportTopLevel
    what do these two do?
provisionResource
putCurrentDocument
    printing?
urlToFilePathIfLocal (move to Project?)
-}
