{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unwords" #-}

module Text.Decker.Internal.External
  ( runExternal,
    runExternalArgs,
    runExternalForSVG,
  )
where

import Control.Exception
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text qualified as Text
import Development.Shake
import Relude hiding (id)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Extra (openFile)
import System.Info qualified
import System.Process
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze.Svg11 hiding (path)
import Text.Blaze.Svg11.Attributes hiding (path, style)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Meta (isMetaSet, lookupMetaOrElse, lookupMetaOrFail)
import Text.Pandoc (Meta)

data Option = Option String | InputFile FilePath | OutputFile FilePath
  deriving (Show)

runExternal :: String -> FilePath -> FilePath -> Meta -> IO ()
runExternal tool inPath outPath meta = do
  runExternalArgs tool [] inPath outPath meta

runExternalArgs :: String -> [String] -> FilePath -> FilePath -> Meta -> IO ()
runExternalArgs tool args inPath outPath meta = do
  transient <- transientDir
  case selectProgramDefinition tool meta of
    Just program -> do
      let pipe = lookupMetaOrElse False (program <> ".pipe") meta
      let command :: String =
            lookupMetaOrFail (program <> ".command") meta
      let arguments =
            args
              <> substituteInOut
                inPath
                outPath
                transient
                (lookupMetaOrFail (program <> ".arguments") meta)
      putStrLn $ "# " <> intercalate " " ([command] <> arguments) <> " (for " <> outPath <> ")"
      catch
        -- (callProcess command arguments)
        (call command arguments inPath outPath pipe)
        ( \(SomeException e) -> do
            let help :: String =
                  lookupMetaOrFail (program <> ".help") meta
            throw
              $ ExternalException
              $ "\nexternal tool configured but unable to run: "
              <> tool
              <> "\nfor more information consult: "
              <> help
              <> "\n\n"
              <> show e
        )
    Nothing ->
      liftIO
        $ throw
        $ ExternalException
        $ "\nexternal tool not configured: "
        <> tool
        <> "\n\n"

call command arguments inPath outPath pipe = do
  if pipe
    then runWithRedirection command arguments inPath outPath
    else callProcess command arguments

runWithRedirection :: FilePath -> [String] -> FilePath -> FilePath -> IO ()
runWithRedirection program args inFile outFile = do
  inHandle <- openFile inFile ReadMode
  outHandle <- openFile outFile WriteMode
  (_, _, _, ph) <-
    createProcess
      (proc program args)
        { std_in = UseHandle inHandle,
          std_out = UseHandle outHandle
        }
  -- Wait for the process to complete
  exitCode <- waitForProcess ph
  -- Clean up
  hClose inHandle
  hClose outHandle
  case exitCode of
    ExitSuccess -> return ()
    failure@(ExitFailure e) -> error "failed"

selectProgramDefinition :: String -> Meta -> Maybe Text
selectProgramDefinition tool meta =
  let osId = case System.Info.os of
        "darwin" -> "macos"
        "mingw32" -> "windows"
        _ -> "linux"
      config = "external-tools." <> toText tool
      osConfig = config <> "." <> osId
   in if
        | isMetaSet osConfig meta -> Just osConfig
        | isMetaSet config meta -> Just config
        | otherwise -> Nothing

substituteInOut :: FilePath -> FilePath -> FilePath -> [String] -> [String]
substituteInOut input output transient =
  map
    ( toString
        . Text.replace "${input}" (toText input)
        . Text.replace "${output}" (toText output)
        . Text.replace "${tmpdir}" (toText transient)
        . toText
    )

runExternalForSVG :: String -> FilePath -> FilePath -> Meta -> IO ()
runExternalForSVG tool inPath outPath meta = do
  catch
    (runExternal tool inPath outPath meta)
    (\(SomeException e) -> renderErrorSvg (Just outPath) (show e))

renderErrorSvg :: Maybe String -> String -> IO ()
renderErrorSvg dst msg = do
  putStrLn $ "# ERROR: " <> msg
  case dst of
    Just out -> do
      let svg =
            docTypeSvg ! viewbox "0 0 512 512" $ do
              style "text{font-size:30px;fill:red;}"
              text_ ! x "20" ! y "20" $ toSvg $ toText msg
      let bytes = renderMarkup svg
      B.writeFile out bytes
    Nothing -> return ()
