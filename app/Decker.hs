{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
import Common
import Exception
import External
import Flags (hasPreextractedResources)
import Output
import Pdf
import Project
import Resources
import Shake
import Utilities

import Control.Exception
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.Extra
import Dachdecker
import Data.Aeson
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Data.Version
import Development.Shake
import Development.Shake.FilePath
import GHC.Conc (numCapabilities)
import System.Decker.OS (defaultProvisioning)
import System.Directory (createDirectoryIfMissing, createFileLink, removeFile)
import System.Environment.Blank
import System.FilePath ()
import Text.Groom
import qualified Text.Mustache as M ()
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Printf (printf)

main :: IO ()
main = do
  when isDevelopmentVersion $
    printf
      "WARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag
  extractResources
  directories <- projectDirectories
  --
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ (show serverPort)
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
      need ["support"]
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
        let ind = replaceSuffix "-deck.html" "-deck-index.yaml" out
        markdownToHtmlDeck src out ind
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
        dot [("-o" ++ out), src]
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
      text <- liftIO $ getResourceString "template/help-page.md"
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
    phony "support" $ do
      metaData <- metaA
      unlessM (Development.Shake.doesDirectoryExist (directories ^. support)) $ do
        liftIO $ createDirectoryIfMissing True (directories ^. public)
        case metaValueAsString "provisioning" metaData of
          Just value
            | value == show SymLink ->
              liftIO $
              createFileLink
                ((directories ^. appData) </> "support")
                (directories ^. support)
          Just value
            | value == show Copy ->
              liftIO $
              copyDir
                ((directories ^. appData) </> "support")
                (directories ^. support)
          Nothing ->
            liftIO $
            case defaultProvisioning of
              SymLink ->
                createFileLink
                  ((directories ^. appData) </> "support")
                  (directories ^. support)
              _ ->
                copyDir
                  ((directories ^. appData) </> "support")
                  (directories ^. support)
          _ -> return ()
    --
    phony "check" checkExternalPrograms
    --
    phony "publish" $ do
      need ["support"]
      allHtmlA >>= need
      metaData <- metaA
      need ["index"]
      let host = metaValueAsString "rsync-destination.host" metaData
      let path = metaValueAsString "rsync-destination.path" metaData
      if isJust host && isJust path
        then do
          let src = (directories ^. public) ++ "/"
          let dst = intercalate ":" [fromJust host, fromJust path]
          ssh [(fromJust host), "mkdir -p", (fromJust path)]
          rsync [src, dst]
        else throw RsyncUrlException
    --
    phony "sync" $ uploadQuizzes (_sources <$> targetsA)
