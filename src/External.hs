{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module External
  ( ssh
  , rsync
  , External.unzip
  , dot
  , gnuplot
  , pdflatex
  , pdf2svg
  , decktape
  , checkExternalPrograms
  ) where

import Exception

import Control.Exception
import Data.Maybe
import Development.Shake
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
        (helpText "`ssh` program (https://www.openssh.com)"))
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
        (helpText "`rsync` program (https://rsync.samba.org)"))
  , ( "unzip"
    , ExternalProgram
        []
        "unzip"
        []
        []
        (helpText "`unzip` program (http://www.info-zip.org)"))
  , ( "dot"
    , ExternalProgram
        []
        "dot"
        ["-Tsvg"]
        ["-V"]
        (helpText "Graphviz package (http://www.graphviz.org)"))
  , ( "gnuplot"
    , ExternalProgram
        []
        "gnuplot"
        ["-d", "-e", "set terminal svg enhanced mouse"]
        ["-V"]
        (helpText "Gnuplot package (http://gnuplot.sourceforge.net)"))
  , ( "pdflatex"
    , ExternalProgram
        []
        "pdflatex"
        ["-halt-on-error", "-interaction=batchmode", "-no-shell-escape"]
        ["--version"]
        (helpText "LaTeX type setter (https://www.tug.org/texlive/)"))
  , ( "pdf2svg"
    , ExternalProgram
        []
        "pdf2svg"
        []
        []
        (helpText "LaTeX type setter (https://github.com/dawbarton/pdf2svg)"))
  , ( "decktape"
    , ExternalProgram
        []
        "decktape"
        ["reveal"]
        []
        (helpText
           "Decktape PDF exporter (https://github.com/astefanutti/decktape)"))
  ]

type Program = [String] -> Action ()

type Program' = [String] -> Action String

ssh :: Program
ssh = makeProgram "ssh"

rsync :: Program
rsync = makeProgram "rsync"

unzip :: Program
unzip = makeProgram "unzip"

dot :: Program
dot = makeProgram "dot"

gnuplot :: Program
gnuplot = makeProgram "gnuplot"

pdflatex :: Program
pdflatex = makeProgram "pdflatex"

pdf2svg :: Program
pdf2svg = makeProgram "pdf2svg"

decktape :: Program
decktape = makeProgram "decktape"

helpText :: String -> String
helpText name =
  "The " ++
  name ++
  " could not be found. Make sure it is installed and available via the `PATH` environment variable."

makeProgram :: String -> ([String] -> Action ())
makeProgram name =
  let external = fromJust $ lookup name programs
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

checkProgram :: String -> Action Bool
checkProgram name =
  liftIO $
  handle (\(SomeException _) -> return False) $ do
    let external = fromJust $ lookup name programs
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
