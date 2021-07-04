{-# LANGUAGE MultiWayIf #-}
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
import Data.Time (diffUTCTime)
import Data.Version
import Development.Shake
import GHC.IO.Encoding
import NeatInterpolation
import qualified System.Directory as Dir
import System.FilePath.Posix
import System.IO
import Text.Decker.Exam.Question
import Text.Decker.Exam.Render
import Text.Decker.Exam.Xml
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.Decker.Writer.Html
import Text.Decker.Writer.Pdf
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc hiding (lookupMeta)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setProjectDirectory
  run

type ParamCache a = FilePath -> Action a

type Cache a = Action a

prepCaches :: Rules (Cache Meta, Cache Targets, ParamCache (Template Text.Text))
prepCaches = do
  getGlobalMeta <- ($ deckerMetaFile) <$> newCache readDeckerMeta
  getTargets <- ($ targetsFile) <$> newCache readTargetsFile
  getTemplate <-
    newCache
      ( \path -> do
          meta <- getGlobalMeta
          readTemplate meta path
      )
  targetsFile %> \targetFile -> do
    alwaysRerun
    meta <- getGlobalMeta
    scanTargetsToFile meta targetFile
  return (getGlobalMeta, getTargets, getTemplate)

needSel sel = needSels [sel]

needSels sels targets = need (concatMap (targets ^.) sels)

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
          # To use 'decker pdf' Google Chrome has to be installed.
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
    want ["decks"]
    addHelpSuffix "For additional information see: https://go.uniwue.de/decker-wiki"
    --
    withTargetDocs "Print version information." $
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
    withTargetDocs "Build HTML versions of all question (*-quest.md)." $
      phony "questions" $ do
        need ["support"]
        getTargets >>= needSel questions
    --
    withTargetDocs "Build HTML versions of all decks (*-deck.md)." $
      phony "decks" $ do
        meta <- getGlobalMeta
        need ["support"]
        getTargets >>= needSel decks
    --
    withTargetDocs "Build HTML versions of all decks, pages and handouts (*-deck.md, *-page.md)." $
      phony "html" $ do
        need ["support"]
        getTargets >>= needSels [decks, pages, handouts]
    --
    withTargetDocs "Build PDF versions of all decks (*-deck.md)." $
      phony "pdf" $ do
        putNormal pdfMsg
        need ["support"]
        getTargets >>= needSel decksPdf
    --
    withTargetDocs "DEPRECATED. Use '--watch'." $
      phony "watch" $ do
        putNormal "Target 'watch' is DEPRECATED. Please use option '--watch'."
        watchChangesAndRepeat
        need ["html"]
    --
    withTargetDocs "DEPRECATED." $
      phony "open" $ do
        putNormal "Target 'open' is DEPRECATED. Please open the browser yourself."
        need ["html"]
        openBrowser indexFile
    --
    withTargetDocs "DEPRECATED. Use '--server'." $
      phony "server" $ do
        putNormal "Target 'server' is DEPRECATED. Please use option '--server'."
        need ["watch", "support"]
        runHttpServer serverPort Nothing
    --
    withTargetDocs "DEPRECATED. Use '--server'." $
      phony "presentation" $ do
        putNormal "Target 'presentation' is DEPRECATED. Please use option '--server'."
        need ["support"]
        runHttpServer serverPort Nothing
        liftIO waitForYes
    --
    withTargetDocs "DEPRECATED. Use '--server'." $
      phony "fast" $ do
        putNormal "Target 'fast' is DEPRECATED. Please use option '--server'."
        watchChangesAndRepeat
        need ["support"]
        runHttpServer serverPort Nothing
        pages <- currentlyServedPages
        need $ map (publicDir </>) pages
    --
    priority 4 $ do
      publicDir <//> "*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        need [src]
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
      publicDir <//> "*-recording.vtt" %> \out -> do
        let src = makeRelative publicDir out
        putNormal $ "# copy (for " <> out <> ")"
        copyFile' src out
      --
      publicDir <//> "*.css" %> \out -> do
        let src = makeRelative publicDir out
        putNormal $ "# copy (for " <> out <> ")"
        copyFile' src out
        whenM (liftIO $ Dir.doesFileExist (src <.> "map")) $
          copyFile' (src <.> "map") (out <.> "map")
      --
      publicDir <//> "*-quest.html" %> \out -> do
        src <- calcSource "-quest.html" "-quest.yaml" out
        meta <- getGlobalMeta
        renderQuestion meta src out
      --
      privateDir <//> "quest-catalog.html" %> \out -> do
        meta <- getGlobalMeta
        targets <- getTargets
        sources <- mapM (calcSource "-quest.html" "-quest.yaml") (targets ^. questions)
        need sources
        renderCatalog meta sources out
      --
      privateDir <//> "quest-catalog.xml" %> \out -> do
        targets <- getTargets
        sources <- mapM (calcSource "-quest.html" "-quest.yaml") (targets ^. questions)
        need sources
        questions <- liftIO $ mapM readQuestion sources
        renderXmlCatalog questions out

      phony "catalogs" $ do
        need ["private/quest-catalog.html", "private/quest-catalog.xml"]
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
      "**/*.css" %> \out -> do
        let src = out -<.> "scss"
        whenM (liftIO $ Dir.doesFileExist src) $ do
          need [src]
          command [] "sass" [src, out]
      --
      "**/*.plantuml.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        putNormal $ "# plantuml (for " <> out <> ")"
        plantuml [src]
        liftIO $ Dir.renameFile (src -<.> "svg") out
      --
      "**/*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        putNormal $ "# dot (for " <> out <> ")"
        dot ["-o" ++ out, src]
      --
      "**/*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        putNormal $ "# gnuplot (for " <> out <> ")"
        gnuplot ["-e", "\"set output '" ++ out ++ "'\"", src]
      --
      "**/*-recording.mp4" %> \out -> do
        let src = replaceSuffix "-recording.mp4" "-recording.webm" out
        let runFfmpeg =
              command [] "ffmpeg" ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", out]
        webmExists <- liftIO $ Dir.doesFileExist src
        when (webmExists) $ do
          need [src]
          mp4Exists <- liftIO $ Dir.doesFileExist out
          if not mp4Exists
            then runFfmpeg
            else do
              let modTime = liftIO . Dir.getModificationTime
              mp4Mod <- modTime out
              webmMod <- modTime src
              when (diffUTCTime webmMod mp4Mod > 0.0) runFfmpeg
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
    withTargetDocs "Copy static file to public dir." $
      phony "static-files" $ do
        targets <- getTargets
        need (targets ^. static)
    --
    withTargetDocs "Provide information about project parameters, sources and targets" $
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
    withTargetDocs "Copy runtime support files to public dir." $
      phony "support" $ do
        need [indexFile, "static-files", "uploads"]
        meta <- getGlobalMeta
        writeSupportFilesToPublic meta
    --
    withTargetDocs "Copy uploaded files to public dir." $
      phony "uploads" $ do
        targets <- getTargets
        need $ targets ^. annotations <> targets ^. times <> targets ^. recordings <> targets ^. captions
    --
    withTargetDocs "Check availability of external programs." $
      phony "check" checkExternalPrograms
    --
    withTargetDocs "Publish the public dir to the configured destination using rsync." $
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
