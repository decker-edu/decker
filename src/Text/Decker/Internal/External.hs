{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unwords" #-}

module Text.Decker.Internal.External
  ( ssh,
    rsync,
    dot,
    plantuml,
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
import qualified Data.ByteString.Lazy as B
import Data.List (lookup)
import qualified Data.List as List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath (takeDirectory)
import Relude
import System.Console.ANSI
import qualified System.Directory as Dir
import System.Exit
import System.Process
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Text.Blaze.Svg11 (docType)
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
        [ "--recursive", "--copy-links", "--delete" ]
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
   in ( \arguments dst -> do
          context <- actionContext
          let status = context ^. externalStatus
          if fromMaybe False (lookup name status)
            then do
              let command = intercalate " " $ [path external] <> args external <> arguments
              putNormal $ "# " ++ command
              liftIO $ callCommand command
            else do
              case dst of
                Just out -> do
                  putError $ "External program '" <> name <> "' is not available. Rendering error to '" <> out <> "'."
                  let bytes = renderMarkup docType
                  liftIO $ B.writeFile out bytes
                Nothing ->
                  putError $ "External program '" <> name <> "' is not available."
      )

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
  exists <- Dir.doesFileExist externalStatusFile
  when exists $ Dir.removeFile externalStatusFile
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
          putStrLn $
            "  "
              ++ setSGRCode [SetColor Foreground Vivid Blue]
              ++ name
              ++ setSGRCode [Reset]
              ++ ": "
              ++ setSGRCode [SetColor Foreground Vivid Green]
              ++ "found"
              ++ setSGRCode [Reset]
        else
          putStrLn $
            "  "
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
  exists <- Dir.doesFileExist externalStatusFile
  if exists
    then do
      fromJust <$> decodeFileStrict externalStatusFile
    else do
      status <- zip (map fst programs) <$> mapM (checkProgram . fst) programs
      Dir.createDirectoryIfMissing True (takeDirectory externalStatusFile)
      encodeFile externalStatusFile status
      return status
