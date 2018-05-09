{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
import Action
import Common
import Context
import Control.Exception
import Control.Monad (when)
import Control.Monad.Extra
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Development.Shake
import Development.Shake.FilePath
import External
import GHC.Conc (numCapabilities)
import Project
import Resources
import System.Directory (createDirectoryIfMissing, removeFile, createFileLink)
import System.FilePath ()
import Text.Groom
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
  let cruft = ["index.md.generated", "log", "//.shake", "generated", "code"]
  context <- makeActionContext dirs
  runShakeInContext context (options projectDir) $
  --
   do
    want ["html"]
    --
    phony "version" $ putNormal $ "decker version " ++ deckerVersion
    --
    phony "decks" $ do
      need ["support"]
      decksA >>= need
    --
    phony "html" $ do
      need ["support"]
      everythingA <++> indexA >>= need
    --
    phony "pdf" $
      decksPdfA <++> pagesPdfA <++> handoutsPdfA <++> indexA >>= need
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
        decktape [(serverUrl </> makeRelative publicDir src), out]
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
      -- deckSources <- deckSourcesA
      -- pageSources <- pageSourcesA
      decks <- decksA
      decksPdf <- decksPdfA
      pagesPdf <- pagesPdfA
      handouts <- handoutsA
      handoutsPdf <- handoutsPdfA
      pages <- pagesA
      -- let deckData =
      --      transpose [deckSources, decks, handouts, decksPdf, handoutsPdf]
      -- let pageData = transpose [pageSources, pages, pagesPdf]
      need $ decks ++ handouts ++ pages
      writeIndexLists
        out
        (takeDirectory index)
        (zip decks decksPdf)
        (zip handouts handoutsPdf)
        (zip pages pagesPdf)
      -- writeIndexTable out (takeDirectory index) deckData pageData
      -- writeIndex out (takeDirectory index) decks handouts pages
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
    priority 2 $
      "//*.css" %> \out -> do
        let src = out -<.> ".scss"
        exists <- doesFileExist src
        when exists $ do
          need [src]
          sassc [src, out]
    --
    phony "clean" $ do
      removeFilesAfter publicDir ["//"]
      removeFilesAfter projectDir cruft
      when isDevelopmentVersion $ removeFilesAfter appDataDir ["//"]
    --
    phony "help" $ do
      text <- liftIO $ getResourceString "template/help-page.md"
      liftIO $ putStr text
    --
    phony "plan" $ do
      metaData <- readMetaDataForDir projectDir
      putNormal $ "\nproject directory: " ++ projectDir
      putNormal $ "public directory: " ++ publicDir
      putNormal $ "support directory: " ++ supportDir
      putNormal $ "application data directory: " ++ appDataDir
      putNormal "\nmeta:\n"
      metaA >>= mapM_ putNormal
      putNormal "\nsources:\n"
      allSourcesA >>= mapM_ putNormal
      putNormal "\ntargets:\n"
      everythingA <++> everythingPdfA >>= mapM_ putNormal
      putNormal "\ntop level meta data:\n"
      putNormal $ groom metaData
    --
    phony "support" $ do
      metaData <- readMetaDataForDir projectDir
      unlessM (Development.Shake.doesDirectoryExist supportDir) $ do
        liftIO $ createDirectoryIfMissing True publicDir
        case metaValueAsString "provisioning" metaData of
          Just value
            | value == show SymLink ->
              liftIO $ createFileLink (appDataDir </> "support") supportDir
          Just value
            | value == show Copy ->
              rsync [(appDataDir </> "support/"), supportDir]
          Nothing ->
            liftIO $ createFileLink (appDataDir </> "support") supportDir
          _ -> return ()
    --
    phony "check" checkExternalPrograms
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
          ssh [(fromJust host), "mkdir -p", (fromJust path)]
          rsync [src, dst]
        else throw RsyncUrlException

-- | Some constants that might need tweaking
options :: FilePath -> ShakeOptions
options projectDir =
  shakeOptions
  { shakeFiles = ".shake"
  , shakeColor = True
  , shakeThreads = numCapabilities
  , shakeAbbreviations = [(projectDir ++ "/", "")]
  }
