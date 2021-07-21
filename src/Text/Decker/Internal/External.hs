{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  )
where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (lookup)
import qualified Data.List as List
import Data.Maybe
import Development.Shake
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
        [ "--recursive",
          "--no-group",
          "--perms",
          "--chmod=a+r,go-w",
          "--no-owner",
          "--copy-links"
        ]
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
          status <- _externalStatus . _state <$> actionContext
          if fromMaybe False (lookup name status)
            then do
              let command = intercalate " " $ [path external] <> args external <> arguments
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
  liftIO $
    handle (\(SomeException _) -> return False) $ do
      let external = fromJust $ List.lookup name programs
      (code, _, _) <-
        readProcessWithExitCode (path external) (testArgs external) ""
      case code of
        ExitFailure status
          | status == 127 -> return False
        _ -> return True

checkExternalPrograms :: IO [(String, Bool)]
checkExternalPrograms = do
  exists <- liftIO $ Dir.doesFileExist externalStatusFile
  if exists
    then do
      fromJust <$> liftIO (decodeFileStrict externalStatusFile)
    else do
      putStrLn "# external programs:"
      status <- zip (map fst programs) <$> mapM check programs
      liftIO $ encodeFile externalStatusFile status
      return status
  where
    check (name, external) = do
      result <- checkProgram name
      if result
        then do
          putStrLn $
            "  "
              ++ setSGRCode [SetColor Foreground Vivid Blue]
              ++ name
              ++ setSGRCode [Reset]
              ++ ": "
              ++ setSGRCode [SetColor Foreground Vivid Green]
              ++ "found"
              ++ setSGRCode [Reset]
          return True
        else do
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
              ++ help external
              ++ ")"
          return False
