{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unwords" #-}

module Text.Decker.Internal.External
  ( ssh,
    rsync,
    dot,
    plantuml,
    mermaid,
    gnuplot,
    pdflatex,
    pdf2svg,
    ffmpeg,
    checkExternalPrograms,
    forceCheckExternalPrograms,
  )
where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.List (lookup)
import Data.List qualified as List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath (takeDirectory)
import Relude hiding (id)
import System.Console.ANSI
import System.Directory qualified as Dir
import System.Exit
import System.Process
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze.Svg11 hiding (path) 
import Text.Blaze.Svg11.Attributes hiding (path)
import Text.Decker.Internal.Common
import Text.Decker.Project.ActionContext

data ExternalProgram = ExternalProgram
  { -- options :: [CmdOption],
    path :: String,
    args :: [String],
    testArgs :: [String],
    help :: String
  }

programs :: [(String, ExternalProgram)]
programs =
  [ ( "ssh",
      ExternalProgram
        -- []
        "ssh"
        []
        ["-V"]
        (helpText "`ssh` (https://www.openssh.com)")
    ),
    ( "rsync",
      ExternalProgram
        -- []
        "rsync"
        []
        ["--version"]
        (helpText "`rsync` (https://rsync.samba.org)")
    ),
    ( "dot",
      ExternalProgram
        -- []
        "dot"
        ["-Tsvg"]
        ["-V"]
        (helpText "Graphviz (http://www.graphviz.org)")
    ),
    ( "plantuml",
      ExternalProgram
        -- []
        "plantuml"
        ["-tsvg"]
        ["--version"]
        (helpText "Plantuml (https://plantuml.com)")
    ),
    ( "mmdc",
      ExternalProgram
        -- []
        "mmdc"
        []
        ["-V"]
        (helpText "Mermaid (https://mermaid.js.org)")
    ),
    ( "gnuplot",
      ExternalProgram
        -- []
        "gnuplot"
        ["-d", "-e", "\"set terminal svg\""]
        ["-V"]
        (helpText "Gnuplot (http://gnuplot.sourceforge.net)")
    ),
    ( "pdflatex",
      ExternalProgram
        -- []
        "pdflatex"
        ["-halt-on-error", "-interaction=batchmode", "-no-shell-escape"]
        ["--version"]
        (helpText "LaTeX (https://www.tug.org/texlive/)")
    ),
    ( "pdf2svg",
      ExternalProgram
        -- []
        "pdf2svg"
        []
        []
        (helpText "LaTeX (https://github.com/dawbarton/pdf2svg)")
    ),
    ( "ffmpeg",
      ExternalProgram
        -- []
        "ffmpeg"
        []
        ["--help"]
        (helpText "FFMpeg (https://ffmpeg.org)")
    )
  ]

type Program = [String] -> Maybe FilePath -> Action ()

ssh :: Program
ssh = makeProgram "ssh"

rsync :: Program
rsync = makeProgram "rsync"

dot :: Program
dot = makeProgram "dot"

plantuml :: Program
plantuml = makeProgram "plantuml"

mermaid :: Program
mermaid = makeProgram "mmdc"

gnuplot :: Program
gnuplot = makeProgram "gnuplot"

pdflatex :: Program
pdflatex = makeProgram "pdflatex"

pdf2svg :: Program
pdf2svg = makeProgram "pdf2svg"

ffmpeg :: Program
ffmpeg = makeProgram "ffmpeg"

helpText :: String -> String
helpText name = name ++ " reported a problem:"

{--
makeProgram' :: String -> ([String] -> Action ())
makeProgram' name =
  let external = fromJust $ List.lookup name programs
   in (\arguments -> do
         (Exit code, Stdout out, Stderr err) <-
           command
             (options external)
             (path external)
             (args external ++ arguments)
         case code of
           ExitSuccess -> return ()
           ExitFailure _ ->
             throw $
             ExternalException $
             "\n" ++ help external ++ "\n\n" ++ err ++ "\n\n" ++ out)
--}

makeProgram :: String -> [String] -> Maybe FilePath -> Action ()
makeProgram name =
  let external = fromJust $ List.lookup name programs
   in (\arguments dst -> do
          context <- actionContext
          let status = context ^. externalStatus
          if fromMaybe False (lookup name status)
            then do
              let command = intercalate " " $ [path external] <> args external <> arguments
              putNormal $ "# " ++ command
              liftIO $ catch (callCommand command) (\(SomeException e) -> renderErrorSvg dst (show e))
            else liftIO $ renderErrorSvg dst $ "External program '" <> name <> "' is not installed."
      )

renderErrorSvg :: Maybe String -> String -> IO ()
renderErrorSvg dst msg = do
    putStrLn $ "# ERROR: " <> msg
    case dst of
        Just out -> do
            let svg = docTypeSvg $ text_ ! x "20" ! y "20" ! class_ "inline-error" $ toSvg $ toText msg
            let bytes = renderMarkup svg
            B.writeFile out bytes
        Nothing -> return ()

checkProgram :: String -> IO Bool
checkProgram name =
  handle (\(SomeException _) -> return False) $ do
    let external = fromJust $ List.lookup name programs
    (code, _, _) <-
      readProcessWithExitCode (path external) (testArgs external) ""
    case code of
      ExitFailure status
        | status == 127 -> return False
      _ -> return True

forceCheckExternalPrograms :: IO ()
forceCheckExternalPrograms = do
  external <- externalStatusFile
  exists <- Dir.doesFileExist external
  when exists $ Dir.removeFile external
  checkExternalPrograms
    >>= printExternalPrograms

printExternalPrograms :: [(String, Bool)] -> IO ()
printExternalPrograms status = do
  putStrLn "# external programs:"
  mapM_ print programs
  where
    print (name, info) = do
      let found = isJust $ lookup name status
      if found
        then
          putStrLn
            $ "  "
            ++ setSGRCode [SetColor Foreground Vivid Blue]
            ++ name
            ++ setSGRCode [Reset]
            ++ ": "
            ++ setSGRCode [SetColor Foreground Vivid Green]
            ++ "found"
            ++ setSGRCode [Reset]
        else
          putStrLn
            $ "  "
            ++ setSGRCode [SetColor Foreground Vivid Blue]
            ++ name
            ++ setSGRCode [Reset]
            ++ ": "
            ++ setSGRCode [SetColor Foreground Vivid Red]
            ++ "missing"
            ++ setSGRCode [Reset]
            ++ " ("
            ++ help info
            ++ ")"

checkExternalPrograms :: IO [(String, Bool)]
checkExternalPrograms = do
  external <- externalStatusFile
  exists <- Dir.doesFileExist external
  if exists
    then do
      fromJust <$> decodeFileStrict external
    else do
      status <- zip (map fst programs) <$> mapM (checkProgram . fst) programs
      Dir.createDirectoryIfMissing True (takeDirectory external)
      encodeFile external status
      return status
