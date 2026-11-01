{-# LANGUAGE OverloadedStrings #-}

-- | Runs the decker executable built by stack on temporary projects. The rules
-- live in the executable, not in the library, so builds can only be tested
-- this way.
module DeckerRun
  ( Run (..),
    deckerExecutable,
    withProject,
    copyProject,
    runDecker,
    buildTargets,
    targetsOf,
    codes,
  )
where

import Control.Exception (finally)
import Control.Monad (filterM, forM, forM_)
import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 qualified as BS
import Data.List (isInfixOf, isSuffixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.Exit (ExitCode)
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Decker.Internal.Common (mkTmpDirName)
import Text.Decker.Internal.Diagnostics (Diagnostic (..))

data Run = Run
  { runExit :: ExitCode,
    runStdout :: String,
    runStderr :: String,
    runDiagnostics :: [Diagnostic]
  }

-- | The decker built by stack. @stack test@ puts it first on the PATH. Refuse
-- any other one, an installed decker would test the wrong version.
deckerExecutable :: IO FilePath
deckerExecutable = do
  found <- findExecutable "decker"
  case found of
    Just exe | ".stack-work" `isInfixOf` exe -> return exe
    other ->
      fail $
        "Need the decker executable built by stack on the PATH (run the tests with `stack test`), found: "
          <> show other

-- | Runs an action on a fresh project directory with the given files, plus an
-- empty @decker.yaml@ if there is none (a build needs one). The directory and
-- decker's transient directory for it are removed afterwards.
withProject :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withProject files action =
  withSystemTempDirectory "decker-test" $ \tmp -> do
    dir <- canonicalizePath tmp
    let config = [("decker.yaml", "{}\n") | "decker.yaml" `notElem` map fst files]
    forM_ (config <> files) $ \(path, content) -> do
      createDirectoryIfMissing True (takeDirectory (dir </> path))
      T.writeFile (dir </> path) content
    temp <- getTemporaryDirectory
    action dir `finally` removePathForcibly (mkTmpDirName temp dir)

-- | Reads all files of a fixture directory, with paths relative to it.
copyProject :: FilePath -> IO [(FilePath, Text)]
copyProject fixture = do
  files <- listFiles fixture
  forM files $ \file -> (makeRelative fixture file,) <$> T.readFile file
  where
    listFiles dir = do
      entries <- map (dir </>) <$> listDirectory dir
      dirs <- filterM doesDirectoryExist entries
      files <- filterM doesFileExist entries
      nested <- concat <$> mapM listFiles dirs
      return (files <> nested)

runDecker :: FilePath -> [String] -> IO Run
runDecker dir args = do
  exe <- deckerExecutable
  (exit, out, err) <- readProcessWithExitCode exe (("--project-dir=" <> dir) : args) ""
  let diagnostics =
        mapMaybe
          (decodeStrict . BS.drop (BS.length marker))
          (filter (marker `BS.isPrefixOf`) (BS.lines (T.encodeUtf8 (T.pack err))))
  return (Run exit out err diagnostics)
  where
    marker = "decker-diagnostic: "

-- | Builds the targets of the project's sources in strict mode with JSON
-- diagnostics.
buildTargets :: FilePath -> [FilePath] -> IO Run
buildTargets dir sources =
  runDecker dir (["--strict", "--diagnostics-json"] <> targetsOf sources)

-- | The build targets of deck, page and question sources.
targetsOf :: [FilePath] -> [FilePath]
targetsOf = concatMap target
  where
    target path
      | "-deck.md" `isSuffixOf` path = ["public/" <> replace "-deck.md" "-deck.html" path]
      | "-page.md" `isSuffixOf` path = ["public/" <> replace "-page.md" "-page.html" path]
      | "-quest.yaml" `isSuffixOf` path = ["private/" <> replace "-quest.yaml" "-quest.html" path]
      | otherwise = []
    replace :: String -> String -> FilePath -> FilePath
    replace suffix new path = take (length path - length suffix) path <> new

-- | The distinct diagnostic codes of a run.
codes :: Run -> [Text]
codes = sort . nub . map diagCode . runDiagnostics
