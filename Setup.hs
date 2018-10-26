import Codec.Archive.Zip
import Conduit
import Control.Monad.Extra
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.IO.Extra

-- main = defaultMain
main = defaultMainWithHooks simpleUserHooks {postCopy = appendResourceArchive}

resourceDir = "./resource"

appendResourceArchive ::
     Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appendResourceArchive args flags descr info = do
  let binDir = fromPathTemplate $ bindir $ installDirTemplates info
  executable <- makeAbsolute $ binDir </> "decker"
  withCurrentDirectory resourceDir $ do
    files <-
      glob "**/*" >>= filterM doesFileExist >>=
      mapM makeRelativeToCurrentDirectory
    withTempFile
      (\archive -> do
         createArchive archive $ forM_ files addFile
         putStrLn $
           "Appending resource archive (" ++
           show (length files) ++ " files) to " ++ executable
         runConduitRes $
           sourceFileBS archive .| sinkIOHandle (openFile executable AppendMode))
  where
    addFile path = do
      selector <- mkEntrySelector path
      loadEntry Deflate selector path