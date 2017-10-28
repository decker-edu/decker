import Distribution.Simple

main = defaultMain
{--
TODO: I cannot figure out exactly why the postReg and postInst hooks are never called. Other than that, this works.

import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.FilePath.Posix
import System.IO

main = defaultMainWithHooks simpleUserHooks {postReg = appendResourcesHook}

appendResourcesHook ::
     Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appendResourcesHook args flags descr info = do
  let deckerBuildPath =
        (fromJust $ flagToMaybe $ regDistPref flags) </> "build" </> "decker" </>
        "decker"
  -- appendResourcesArchive deckerBuildPath
  putStrLn $
    "### Append the shit! " ++ "(" ++ deckerBuildPath ++ ")" ++ show flags

appendResourcesArchive :: FilePath -> IO ()
appendResourcesArchive executable = do
  resourceArchive <-
    withCurrentDirectory "./resource" $ do
      addFilesToArchive [OptRecursive] emptyArchive ["support"]
  B.appendFile executable (fromArchive resourceArchive)
--}
