{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Decker where

import Control.Concurrent
import Control.Lens ((^.))
import Control.Monad.Extra
import Data.IORef ()
import Data.List
import Data.String ()
import qualified Data.Text as Text
import Data.Version
import Development.Shake
import GHC.IO.Encoding
import NeatInterpolation
import qualified System.Directory as Dir
import System.Environment.Blank
import System.FilePath.Posix
import System.IO
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template
import Text.Decker.Writer.Html
import Text.Decker.Writer.Pdf
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc hiding (lookupMeta)

main :: IO ()
main = do
  setLocaleEncoding utf8
  startDir <- Dir.getCurrentDirectory
  setProjectDirectory
  args <- getArgs
  if null args
    then run
    else case head args of
      "example" -> writeExampleProject startDir
      "clean" -> runClean
      _ -> run

type ParamCache a = FilePath -> Action a

type Cache a = Action a

prepCaches :: Rules (Cache Meta, Cache Targets, ParamCache (Template Text.Text))
prepCaches = do
  let deckerMetaFile = "decker.yaml"
  let deckerTargetsFile = transientDir </> "targets.yaml"
  getGlobalMeta <- ($ deckerMetaFile) <$> newCache readDeckerMeta
  getTargets <- ($ deckerTargetsFile) <$> newCache readTargetsFile
  getTemplate <-
    newCache
      ( \path -> do
          meta <- getGlobalMeta
          readTemplate meta path
      )
  deckerTargetsFile %> \targetFile -> do
    alwaysRerun
    meta <- getGlobalMeta
    scanTargetsToFile meta targetFile
  return (getGlobalMeta, getTargets, getTemplate)

needSel sel = needSels [sel]

needSels sels targets = need (concatMap (targets ^.) sels)

-- | Functionality for "decker clean" command Removes public and .decker
-- directory. Located outside of Shake due to unlinking differences and
-- parallel processes on Windows which prevented files (.shake.lock) from being
-- deleted on Windows.
runClean :: IO ()
runClean = do
  warnVersion
  putStrLn $ "# Removing " ++ publicDir
  tryRemoveDirectory publicDir
  putStrLn $ "# Removing " ++ transientDir
  tryRemoveDirectory transientDir

run :: IO ()
run = do
  warnVersion
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ show serverPort
  let indexSource = "index.md"
  let generatedIndexSource = transientDir </> "index.md.generated"
  let indexFile = publicDir </> "index.html"
  let pdfMsg =
        Text.unpack
          [text|
          # 
          # To use 'decker pdf' or 'decker pdf-decks', Google Chrome has to be
          # installed.
          # 
          # Windows: Currently 'decker pdf' does not work on Windows.
          #   Please add 'print: true' or 'menu: true' to your slide deck and use
          #   the print button on the title slide.
          #
          # MacOS: Follow the Google Chrome installer instructions.
          #   'Google Chrome.app' has to be located in either of these locations
          #
          #   - '/Applications/Google Chrome.app' 
          #   - '/Users/<username>/Applications/Google Chrome.app'
          #
          # Linux: 'chrome' has to be on $$PATH.
          # 
        |]
  --
  runDecker $ do
    (getGlobalMeta, getTargets, getTemplate) <- prepCaches
    --
    want ["decks"]
    --

    phony "tutorial" $ do
      putNormal "# To find information on how to use decker please check the documentation in the wiki: https://go.uniwue.de/decker-wiki"
      putNormal "# To create a new project please use the command \"decker example\""
    --
    phony "help" $
      putNormal "# To find information on how to use decker please check the documentation in the wiki: https://go.uniwue.de/decker-wiki"
    --
    phony "version" $ do
      putNormal $
        "decker version "
          ++ deckerVersion
          ++ " (branch: "
          ++ deckerGitBranch
          ++ ", commit: "
          ++ deckerGitCommitId
          ++ ", tag: "
          ++ deckerGitVersionTag
          ++ ", build date: "
          ++ deckerBuildDate
          ++ ")"
      putNormal $ "pandoc version " ++ Text.unpack pandocVersion
      putNormal $ "pandoc-types version " ++ showVersion pandocTypesVersion
    --
    phony "decks" $ do
      need ["support"]
      getTargets >>= needSel decks
    --
    phony "html" $ do
      need ["support"]
      getTargets >>= needSels [decks, pages, handouts]
    --
    phony "pdf" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSel decksPdf
    --
    phony "pdf-decks" $ do
      putNormal pdfMsg
      need ["support"]
      getTargets >>= needSel decksPdf
    --
    phony "watch" $ do
      watchChangesAndRepeat
      need ["html"]
    --
    phony "open" $ do
      need ["html"]
      openBrowser indexFile
    --
    phony "server" $ do
      need ["watch", "support"]
      runHttpServer serverPort Nothing
    --
    phony "presentation" $ do
      need ["support"]
      runHttpServer serverPort Nothing
      liftIO waitForYes
    --
    phony "fast" $ do
      watchChangesAndRepeat
      need ["support"]
      runHttpServer serverPort Nothing
      pages <- currentlyServedPages
      need $ map (publicDir </>) pages
    --
    priority 4 $ do
      publicDir <//> "*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        needIfExists "-deck.html" "-annot.json" out
        needIfExists "-deck.html" "-times.json" out
        -- needIfExists "-deck.html" "-recording.mp4" out
        let recordingWebm = replaceSuffix "-deck.md" "-recording.webm" src
        let recordingMp4 = replaceSuffix "-deck.html" "-recording.mp4" out
        whenM (doesFileExist recordingWebm) $ need [recordingMp4]
        meta <- getGlobalMeta
        markdownToHtmlDeck meta getTemplate src out
      --
      publicDir <//> "*-deck.pdf" %> \out -> do
        let src = replaceSuffix "-deck.pdf" "-deck.html" out
        let url = serverUrl </> makeRelative publicDir src
        need [src]
        runHttpServer serverPort Nothing
        putNormal $ "# chrome started ... (for " <> out <> ")"
        result <- liftIO $ launchChrome url out
        case result of
          Right _ -> putNormal $ "# chrome finished (for " <> out <> ")"
          Left msg -> error msg
      --
      publicDir <//> "*-handout.html" %> \out -> do
        src <- calcSource "-handout.html" "-deck.md" out
        meta <- getGlobalMeta
        markdownToHtmlHandout meta getTemplate src out
      --
      publicDir <//> "*-handout.pdf" %> \out -> do
        src <- calcSource "-handout.pdf" "-deck.md" out
        meta <- getGlobalMeta
        markdownToPdfHandout meta getTemplate src out
      --
      publicDir <//> "*-page.html" %> \out -> do
        src <- calcSource "-page.html" "-page.md" out
        meta <- getGlobalMeta
        markdownToHtmlPage meta getTemplate src out
      --
      publicDir <//> "*-page.pdf" %> \out -> do
        src <- calcSource "-page.pdf" "-page.md" out
        meta <- getGlobalMeta
        markdownToPdfPage meta getTemplate src out
      --
      publicDir <//> "*-recording.mp4" %> \out -> do
        let src = makeRelative publicDir out
        putNormal $ "# copy (for " <> out <> ")"
        copyFile' src out
      --
      indexFile %> \out -> do
        exists <- liftIO $ Dir.doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else generatedIndexSource
        need [src]
        meta <- getGlobalMeta
        markdownToHtmlPage meta getTemplate src out
      --
      generatedIndexSource %> \out -> do
        targets <- getTargets
        meta <- getGlobalMeta
        writeIndexLists meta targets out (takeDirectory indexFile)
    --
    priority 3 $ do
      "**/*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        dot ["-o" ++ out, src]
      --
      "**/*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        gnuplot ["-e", "\"set output '" ++ out ++ "'\"", src]
      --
      "**/*-recording.mp4" %> \out -> do
        let src = replaceSuffix "-recording.mp4" "-recording.webm" out
        need [src]
        -- whenM (doesFileExist src) $
        command [] "ffmpeg" ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", out]
      --
      "**/*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        pdflatex ["-output-directory", dir, src]
        pdf2svg [pdf, out]
        liftIO $ Dir.removeFile pdf
    --
    -- Catch all. Just copy project/* to public/*. This nicely handles ALL
    -- resources. Just `need` them where you need them.
    priority 2 $
      publicDir <//> "//" %> \out -> do
        let src = makeRelative publicDir out
        putNormal $ "# copy (for " <> out <> ")"
        copyFile' src out
    --
    phony "static-files" $ do
      targets <- getTargets
      need (targets ^. static)
    --
    phony "uploads" $ do
      targets <- getTargets
      need (targets ^. uploads)
    --
    phony "info" $ do
      project <- liftIO $ Dir.canonicalizePath projectDir
      putNormal $ "\nproject directory: " ++ project
      putNormal $ "public directory: " ++ publicDir
      putNormal $ "support directory: " ++ supportDir
      meta <- getGlobalMeta
      targets <- getTargets
      templateSource <- liftIO $ calcTemplateSource meta
      putNormal $ "template source: " <> show templateSource
      putNormal "\ntargets:\n"
      putNormal (groom targets)
      putNormal "\ntop level meta data:\n"
      putNormal (groom meta)
    --
    phony "support" $ do
      need [indexFile, "static-files", "uploads"]
      meta <- getGlobalMeta
      writeSupportFilesToPublic meta
    --
    phony "check" checkExternalPrograms
    --
    phony "publish" $ do
      need ["support"]
      meta <- getGlobalMeta
      getTargets >>= needSels [decks, handouts, pages]
      let src = publicDir ++ "/"
      case lookupMeta "publish.rsync.destination" meta of
        Just destination -> publishWithRsync src destination meta
        _ -> do
          let host = lookupMetaOrFail "rsync-destination.host" meta
          let path = lookupMetaOrFail "rsync-destination.path" meta
          let dst = intercalate ":" [host, path]
          ssh [host, "mkdir -p", path]
          rsync [src, dst]

needIfExists :: String -> String -> String -> Action ()
needIfExists suffix also out = do
  let annotDst = replaceSuffix suffix also out
  annotSrc <- calcSource' annotDst
  doesFileExist annotSrc
  exists <- liftIO $ Dir.doesFileExist annotSrc
  when exists $ need [annotDst]

publishWithRsync :: String -> String -> Meta -> Action ()
publishWithRsync source destination meta = do
  let options = lookupMetaOrElse [] "publish.rsync.options" meta :: [String]
  rsync $ options <> [source, destination]

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
