{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.External
  ( ssh
  , rsync
  , dot
  , gnuplot
  , pdflatex
  , pdf2svg
  , ffmpeg
  , checkExternalPrograms
  ) where

import Text.Decker.Internal.Exception

import Control.Exception
import qualified Data.List as List
import Data.Maybe
import Development.Shake
import Relude
import System.Console.ANSI
import System.Exit
import System.Process

data ExternalProgram = ExternalProgram
  { options :: [CmdOption]
  , path :: String
  , args :: [String]
  , testArgs :: [String]
  , help :: String
  }

programs :: [(String, ExternalProgram)]
programs =
  [ ( "ssh"
    , ExternalProgram
        []
        "ssh"
        []
        ["-V"]
        (helpText "`ssh` (https://www.openssh.com)"))
  , ( "rsync"
    , ExternalProgram
        []
        "rsync"
        [ "--recursive"
        , "--no-group"
        , "--perms"
        , "--chmod=a+r,go-w"
        , "--no-owner"
        , "--copy-links"
        ]
        ["--version"]
        (helpText "`rsync` (https://rsync.samba.org)"))
  , ( "dot"
    , ExternalProgram
        []
        "dot"
        ["-Tsvg"]
        ["-V"]
        (helpText "Graphviz (http://www.graphviz.org)"))
  , ( "gnuplot"
    , ExternalProgram
        []
        "gnuplot"
        ["-d", "-e", "\"set terminal svg\""]
        ["-V"]
        (helpText "Gnuplot (http://gnuplot.sourceforge.net)"))
  , ( "pdflatex"
    , ExternalProgram
        []
        "pdflatex"
        ["-halt-on-error", "-interaction=batchmode", "-no-shell-escape"]
        ["--version"]
        (helpText "LaTeX (https://www.tug.org/texlive/)"))
  , ( "pdf2svg"
    , ExternalProgram
        []
        "pdf2svg"
        []
        []
        (helpText "LaTeX (https://github.com/dawbarton/pdf2svg)"))
  ]

type Program = [String] -> Action ()

ssh :: Program
ssh = makeProgram "ssh"

rsync :: Program
rsync = makeProgram "rsync"

dot :: Program
dot = makeProgram "dot"

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

makeProgram :: String -> [String] -> Action ()
makeProgram name =
  let external = fromJust $ List.lookup name programs
   in (\arguments -> do
         let command =
               intercalate " " $ [path external] <> args external <> arguments
         liftIO $ callCommand command)

checkProgram :: String -> Action Bool
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

checkExternalPrograms :: Action ()
checkExternalPrograms = putNormal "# external programs:" >> mapM_ check programs
  where
    check (name, external) = do
      result <- checkProgram name
      if result
        then putNormal $
             "  " ++
             setSGRCode [SetColor Foreground Vivid Blue] ++
             name ++
             setSGRCode [Reset] ++
             ": " ++
             setSGRCode [SetColor Foreground Vivid Green] ++
             "found" ++ setSGRCode [Reset]
        else putNormal $
             "  " ++
             setSGRCode [SetColor Foreground Vivid Blue] ++
             name ++
             setSGRCode [Reset] ++
             ": " ++
             setSGRCode [SetColor Foreground Vivid Red] ++
             "missing" ++ setSGRCode [Reset] ++ " (" ++ help external ++ ")"
