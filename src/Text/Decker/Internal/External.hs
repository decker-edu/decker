{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unwords" #-}

module Text.Decker.Internal.External
  ( runExternal,
    runExternalForSVG,
  )
where

import Control.Exception
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text qualified as Text
import Development.Shake
import Relude hiding (id)
import System.Process
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze.Svg11 hiding (path)
import Text.Blaze.Svg11.Attributes hiding (path, style)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Meta (lookupMetaOrFail, isMetaSet)
import Text.Pandoc (Meta)
import qualified System.Info

data Option = Option String | InputFile FilePath | OutputFile FilePath
  deriving (Show)

runExternal :: String -> FilePath -> FilePath -> Meta -> IO ()
runExternal tool inPath outPath meta = do
  transient <- transientDir
  case selectProgramDefinition tool meta of
    Just program -> do
      let command :: String =
            lookupMetaOrFail (program <> ".command") meta
      let arguments =
            substituteInOut inPath outPath transient
              $ lookupMetaOrFail (program <> ".arguments") meta
      putStrLn $ "# " <> intercalate " " ([command] <> arguments) <> " (for " <> outPath <> ")"
      catch
        (callProcess command arguments)
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

selectProgramDefinition :: String -> Meta -> Maybe Text
selectProgramDefinition tool meta =
    let osId = case System.Info.os of
                "darwin" -> "macos"
                "mingw32" -> "windows"
                _ -> "linux"
        config = "external-tools." <> toText tool
        osConfig = config <> "." <> osId
    in if | isMetaSet osConfig meta -> Just osConfig
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

