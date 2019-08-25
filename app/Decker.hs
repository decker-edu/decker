{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Decker where

import Text.Decker.Internal.Exception
import Text.Decker.Internal.External
import Text.Decker.Internal.Flags (hasPreextractedResources)
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Server.Dachdecker
import Text.Decker.Writer.Format
import Text.Decker.Writer.Html
import Text.Decker.Writer.Pdf

import Control.Exception
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Extra
import Control.Concurrent
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import System.Directory (removeFile)
import System.Environment.Blank
import System.IO
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc
import Text.Printf (printf)

ttt =
  [ "template/deck.html"
  , "template/deck.md"
  , "template/handout.html"
  , "template/handout.tex"
  , "template/page.html"
  , "template/page.tex"
  ]

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 && head args == "format"
    then formatMarkdown
    else run

run :: IO ()
run = do
  when isDevelopmentVersion $
    printf
      "WARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag
  directories <- projectDirectories
  --
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ show serverPort
  let indexSource = (directories ^. project) </> "index.md"
  let index = (directories ^. public) </> "index.html"
  let cruft = ["index.md.generated", "log", "//.shake", "generated", "code"]
  let pdfMsg =
        "\n# To use 'decker pdf' or 'decker pdf-decks', Google Chrome has to be installed.\n" ++
        "# Windows: Currently 'decker pdf' does not work on Windows.\n" ++
        "\tPlease add 'print: true' or 'menu: true' to your slide deck and use the print button on the title slide.\n" ++
        "# MacOS: Follow the Google Chrome installer instructions.\n" ++
        "\tGoogle Chrome.app has to be located in either /Applications/Google Chrome.app or /Users/<username>/Applications/Google Chrome.app\n" ++
        "\tAlternatively you can add 'chrome' to $PATH.\n" ++
        "# Linux: 'chrome' has to be on $PATH.\n"
  --
  runDecker $
  --
   do
    want ["html"]
    --
    phony "version" $ do
      putNormal $
        "decker version " ++
        deckerVersion ++
        " (branch: " ++
        deckerGitBranch ++
        ", commit: " ++
        deckerGitCommitId ++ ", tag: " ++ deckerGitVersionTag ++ ")"
      putNormal $ "pandoc version " ++ pandocVersion
      putNormal $ "pandoc-types version " ++ showVersion pandocTypesVersion
    --
    phony "decks" $ do
      need ["support"]
      decksA >>= need
      need ["index"]
    --
    phony "html" $ do
      need ["support", "publish-annotations"]
      allHtmlA >>= need
      need ["index"]
    --
    phony "pdf" $ do
      putNormal pdfMsg
      need ["support"]
      allPdfA >>= need
      need ["index"]
    --
    phony "pdf-decks" $ do
      putNormal pdfMsg
      need ["support"]
      decksPdfA >>= need
      need ["index"]
    --
    phony "watch" $ do
      need ["html"]
      watchChangesAndRepeat
    --
    phony "open" $ do
      need ["html"]
      openBrowser index
    --
    phony "server" $ do
      need ["watch"]
      runHttpServer serverPort directories Nothing
    --
    phony "presentation" $ do
      runHttpServer serverPort directories Nothing
      liftIO $ waitForYes
    --
    phony "example" $ liftIO writeExampleProject
    --
    phony "tutorial" $ liftIO writeTutorialProject
    --
    phony "sketch-pad-index" $ do
      indicesA >>= need
      indicesA >>=
        writeSketchPadIndex ((directories ^. public) </> "sketch-pad.yaml")
    --
    phony "index" $ need ["support", index]
    --
    priority 2 $
      "//*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        let index = replaceSuffix "-deck.html" "-deck-index.yaml" out
        let annotSrc = replaceSuffix "-deck.md" "-annot.json" src
        let annotDst = replaceSuffix "-deck.html" "-annot.json" out
        exists <- doesFileExist annotSrc
        when exists $ copyFileChanged annotSrc annotDst
        markdownToHtmlDeck src out index
    --
    priority 2 $
      "//*-deck-index.yaml" %> \ind -> do
        src <- calcSource "-deck-index.yaml" "-deck.md" ind
        let out = replaceSuffix "-deck-index.yaml" "-deck.html" ind
        markdownToHtmlDeck src out ind
    --
    priority 2 $
      "//*-deck.pdf" %> \out -> do
        let src = replaceSuffix "-deck.pdf" "-deck.html" out
        need [src]
        putNormal $ "Started: " ++ src ++ " -> " ++ out
        runHttpServer serverPort directories Nothing
        result <-
          liftIO $
          launchChrome
            (serverUrl </> makeRelative (directories ^. public) src)
            out
        case result of
          Right msg -> putNormal msg
          Left msg -> error msg
    --
    priority 2 $
      "//*-handout.html" %> \out -> do
        src <- calcSource "-handout.html" "-deck.md" out
        markdownToHtmlHandout src out
    --
    priority 2 $
      "//*-handout.pdf" %> \out -> do
        src <- calcSource "-handout.pdf" "-deck.md" out
        markdownToPdfHandout src out
    --
    priority 2 $
      "//*-page.html" %> \out -> do
        src <- calcSource "-page.html" "-page.md" out
        markdownToHtmlPage src out
    --
    priority 2 $
      "//*-page.pdf" %> \out -> do
        src <- calcSource "-page.pdf" "-page.md" out
        markdownToPdfPage src out
    --
    priority 2 $
      index %> \out -> do
        alwaysRerun
        exists <- Development.Shake.doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else indexSource <.> "generated"
        markdownToHtmlPage src out
    --
    indexSource <.> "generated" %> \out -> do
      alwaysRerun
      writeIndexLists out (takeDirectory index)
    --
    priority 2 $
      "//*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        dot ["-o" ++ out, src]
    --
    priority 2 $
      "//*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        gnuplot ["-e", "set output \"" ++ out ++ "\"", src]
    --
    priority 2 $
      "//*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        pdflatex ["-output-directory", dir, src]
        pdf2svg [pdf, out]
        liftIO $ removeFile pdf
    --
    phony "clean" $ do
      removeFilesAfter (directories ^. public) ["//"]
      removeFilesAfter (directories ^. project) cruft
      old <- liftIO oldResourcePaths
      forM_ old $ \dir -> removeFilesAfter dir ["//"]
      when (isDevelopmentVersion && not hasPreextractedResources) $
        removeFilesAfter (directories ^. appData) ["//"]
    --
    phony "help" $ do
      text <- getTemplate' "template/help-page.md"
      liftIO $ putStr text
    --
    phony "info" $ do
      putNormal $ "\nproject directory: " ++ (directories ^. project)
      putNormal $ "public directory: " ++ (directories ^. public)
      putNormal $ "support directory: " ++ (directories ^. support)
      putNormal $ "application data directory: " ++ (directories ^. appData)
      putNormal "\ntargets:\n"
      allHtmlA <++> allPdfA >>= mapM_ putNormal
      putNormal "\ntop level meta data:\n"
      groom <$> metaA >>= putNormal
    --
    phony "support" writeSupportFilesToPublic
    --
    phony "check" checkExternalPrograms
    --
    phony "publish-annotations" $ do
      metaData <- metaA
      when (isJust $ metaValueAsString "publish-annotations" metaData) $ do
        let src = (directories ^. project) </> "annotations"
        let dst = (directories ^. public) </> "annotations"
        exists <- doesDirectoryExist src
        when exists $ do
          putNormal $ "# publish annotations (to " ++ dst ++ ")"
          liftIO $ copyDir src dst
    --
    phony "publish" $ do
      need ["support", "sketch-pad-index"]
      allHtmlA >>= need
      metaData <- metaA
      need ["index"]
      let host = metaValueAsString "rsync-destination.host" metaData
      let path = metaValueAsString "rsync-destination.path" metaData
      if isJust host && isJust path
        then do
          let src = (directories ^. public) ++ "/"
          let dst = intercalate ":" [fromJust host, fromJust path]
          ssh [fromJust host, "mkdir -p", fromJust path]
          rsync [src, dst]
        else throw RsyncUrlException
    --
    phony "sync" $ uploadQuizzes (_sources <$> targetsA)

waitForYes :: IO ()
waitForYes = do
  threadDelay 1000
  putStr "\nDecker server running. Push ENTER to terminate."
  hFlush stdout
  _ <- getLine
  putStr "Terminate server? (y/N): "
  hFlush stdout
  input <- getLine
  unless (input == "y") waitForYes
