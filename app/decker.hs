{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
import Action
import Common
import Context
import Control.Exception
import Control.Monad ()
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Development.Shake
import Development.Shake.FilePath
import GHC.Conc (numCapabilities)
import Project
import Resources
import System.Exit
import System.FilePath ()
import qualified Text.Mustache as M ()
import Text.Pandoc ()
import Text.Printf ()
import Utilities

main :: IO ()
main = do
  extractResources
  dirs <- projectDirectories
  --
  let projectDir = project dirs
  let publicDir = public dirs
  let supportDir = support dirs
  let appDataDir = appData dirs
  let serverPort = 8888
  let serverUrl = "http://localhost:" ++ (show serverPort)
  -- Find sources. These are formulated as actions in the Action mondad, such
  -- that each new iteration rescans all possible source files.
  let deckSourcesA = globA "**/*-deck.md"
  let pageSourcesA = globA "**/*-page.md"
  let allSourcesA = deckSourcesA <++> pageSourcesA
  let allMarkdownA = globA "**/*.md"
  let allImagesA = globA "**/*.png" <++> globA "**/*.jpg"
  let metaA = globA "**/*-meta.yaml"
  -- Calculate targets
  let decksA = deckSourcesA >>= calcTargets ".md" ".html"
  let decksPdfA = deckSourcesA >>= calcTargets ".md" ".pdf"
  let handoutsA = deckSourcesA >>= calcTargets "-deck.md" "-handout.html"
  let handoutsPdfA = deckSourcesA >>= calcTargets "-deck.md" "-handout.pdf"
  let pagesA = pageSourcesA >>= calcTargets ".md" ".html"
  let pagesPdfA = pageSourcesA >>= calcTargets ".md" ".pdf"
  let indexSource = project dirs </> "index.md"
  let index = publicDir </> "index.html"
  let indexA = return [index] :: Action [FilePath]
  let everythingA = decksA <++> handoutsA <++> pagesA
  let everythingPdfA = decksPdfA <++> handoutsPdfA <++> pagesPdfA
  let cruft = ["index.md.generated", "log", "//.shake", "generated"]
  context <- makeActionContext dirs
  runShakeInContext context options $
  --
   do
    want ["html"]
    --
    phony "version" $ putNormal $ "decker version " ++ deckerVersion
    --
    phony "decks" $ do
      decksA >>= need
      need ["support"]
    --
    phony "html" $ do
      everythingA <++> indexA >>= need
      need ["support"]
    --
    phony "pdf" $ pagesPdfA <++> handoutsPdfA <++> indexA >>= need
    --
    phony "pdf-decks" $ decksPdfA <++> indexA >>= need
    --
    phony "watch" $ do
      need ["html"]
      allMarkdownA <++> metaA <++> allImagesA >>= watchFiles
    --
    phony "open" $ do
      need ["html"]
      openBrowser index
    --
    phony "server" $ do
      need ["watch"]
      runHttpServer serverPort dirs Nothing
    --
    phony "example" writeExampleProject
    --
    phony "index" $ need [index, "support"]
    --
    priority 2 $
      "//*-deck.html" %> \out -> do
        src <- calcSource "-deck.html" "-deck.md" out
        markdownToHtmlDeck src out
    --
    priority 2 $
      "//*-deck.pdf" %> \out -> do
        let src = replaceSuffix "-deck.pdf" "-deck.html" out
        need [src]
        putNormal $ src ++ " -> " ++ out
        runHttpServer serverPort dirs Nothing
        code <-
          cmd
            "decktape.sh reveal"
            (serverUrl </> makeRelative publicDir src)
            out
        case code of
          ExitFailure _ -> throw $ DecktapeException "Unknown."
          ExitSuccess -> return ()
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
        exists <- Development.Shake.doesFileExist indexSource
        let src =
              if exists
                then indexSource
                else indexSource <.> "generated"
        markdownToHtmlPage src out
    --
    indexSource <.> "generated" %> \out -> do
      decks <- decksA
      handouts <- handoutsA
      pages <- pagesA
      need $ decks ++ handouts ++ pages
      writeIndex out (takeDirectory index) decks handouts pages
    --
    priority 2 $
      "//*.dot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        cmd "dot -Tsvg" ("-o" ++ out) src
    --
    priority 2 $
      "//*.gnuplot.svg" %> \out -> do
        let src = dropExtension out
        need [src]
        cmd
          "gnuplot -d"
          ["-e", "set terminal svg"]
          ["-e", "set output \"" ++ out ++ "\""]
          src
    --
    priority 2 $
      "//*.tex.svg" %> \out -> do
        let src = dropExtension out
        let pdf = src -<.> ".pdf"
        let dir = takeDirectory src
        need [src]
        () <-
          cmd
            "pdflatex -halt-on-error -interaction batchmode"
            ["-output-directory", dir]
            src
        () <- cmd "pdf2svg" pdf out
        cmd "rm" pdf
    --
    phony "clean" $ do
      removeFilesAfter publicDir ["//"]
      removeFilesAfter projectDir cruft
    --
    phony "help" $ do
      text <- liftIO $ getResourceString "template/help-page.md"
      liftIO $ putStr text
    --
    phony "plan" $ do
      putNormal $ "project directory: " ++ projectDir
      putNormal $ "public directory: " ++ publicDir
      putNormal $ "support directory: " ++ supportDir
      putNormal $ "application data directory: " ++ appDataDir
      putNormal "meta:"
      metaA >>= mapM_ putNormal
      putNormal "sources:"
      allSourcesA >>= mapM_ putNormal
      putNormal "targets:"
      everythingA <++> everythingPdfA >>= mapM_ putNormal
    --
    -- phony "support" $ writeEmbeddedFiles deckerSupportDir supportDir
    phony "support" $ do liftIO $ writeResourceFiles "support" supportDir
    --
    phony "publish" $ do
      need ["support"]
      everythingA <++> indexA >>= need
      metaData <- readMetaDataForDir projectDir
      let host = metaValueAsString "rsync-destination.host" metaData
      let path = metaValueAsString "rsync-destination.path" metaData
      if isJust host && isJust path
        then do
          let src = publicDir ++ "/"
          let dst = intercalate ":" [fromJust host, fromJust path]
          cmd "ssh " (fromJust host) "mkdir -p" (fromJust path) :: Action ()
          cmd
            "rsync --recursive --no-xattrs --no-group --perms --chmod=a+r,go-w --no-owner --copy-links"
            src
            dst :: Action ()
        else throw RsyncUrlException

-- Calculate some directories
-- | Some constants that might need tweaking
options :: ShakeOptions
options = shakeOptions {shakeFiles = ".shake", shakeThreads = numCapabilities}
