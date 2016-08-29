{-# LANGUAGE TemplateHaskell #-}

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8      as B
import           Data.FileEmbed
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Yaml.Pretty
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob
import qualified Text.Mustache              as M
import           Text.Mustache.Types        (mFromJSON)
import           Text.Pandoc
import           Text.Printf
import           Utilities

-- | All observable source files that are considered. These are specified in
-- the Action monad, such that they are revealuated on each iteration of the *watch* target.
getDeckSources = globRelative "**/*-deck.md"

getPageSources = globRelative "**/*-page.md"

getAllSources = globRelative "**/*.md"

-- | Calculates all plain markdown files ending just in `*.md`.
getPlainSources =
  do all <- getAllSources
     decks <- getDeckSources
     pages <- getPageSources
     return $ all \\ (decks ++ pages)

-- | Returns all YAML files.
getMeta = globRelative "**/*.yaml"

-- | Actions that generate lists of target files from the source list actions
getDecks = getDeckSources >>= replaceSuffixWith ".md" ".html"
getDecksPdf = getDeckSources >>= replaceSuffixWith ".md" ".pdf"
getHandouts = getDeckSources >>= replaceSuffixWith "-deck.md" "-handout.html"
getHandoutsPdf = getDeckSources >>= replaceSuffixWith "-deck.md" "-handout.pdf"
getPages = getPageSources >>= replaceSuffixWith ".md" ".html"
getPagesPdf = getPageSources >>= replaceSuffixWith ".md" ".pdf"
getPlain = getPlainSources >>= replaceSuffixWith ".md" ".html"
getPlainPdf = getPlainSources >>= replaceSuffixWith ".md" ".pdf"
getEverything = getDecks <++> getHandouts <++> getPages <++> getPlain
getEverythingPdf = getDecksPdf <++> getHandoutsPdf <++> getPagesPdf <++> getPlain

-- | Stuff that will be deleted by the clean target
getCruft = return ["index.md.generated", "index.html", "server.log"]

main = do
    contextRef <-   newIORef defaultContext
    runShakeInContext contextRef options $ do

        want ["html"]

        phony "html" $ do
            need ["index.html"]
            getDecks <++> getHandouts <++> getPages <++> getPlain >>= need

        phony "pdf" $ do
            need ["index.html"]
            getPagesPdf <++> getHandoutsPdf <++> getPlainPdf >>= need

        phony "pdf-decks" $ do
            need ["index.html"]
            getDecksPdf >>= need

        phony "watch" $ do
            need ["html"]
            getDecks <++> getHandouts <++> getPages <++> getPlain >>= markNeeded
            sources <- getAllSources
            meta <- getMeta
            watchFiles (sources ++ meta) contextRef

        phony "server" $ do
            need ["watch"]
            runHttpServer contextRef True

        phony "example" writeExampleProject

        priority 2 $ "//*-deck.html" %> \out -> do
            let src = out -<.> "md"
            meta <- getMeta
            markdownToHtmlDeck src meta out

        priority 2 $ "//*-deck.pdf" %> \out -> do
            let src = out -<.> "html"
            need [src]
            runHttpServer contextRef False
            code <- cmd "decktape.sh reveal" ("http://localhost:8888/" ++ src) out
            case code of
              ExitFailure _ -> do
                 cdnBase <- getBaseUrl
                 throw $ DecktapeException cdnBase
              ExitSuccess ->
                 return ()

        priority 2 $ "//*-handout.html" %> \out -> do
            let src = dropSuffix "-handout.html" out ++ "-deck.md"
            meta <- getMeta
            markdownToHtmlHandout src meta out

        priority 2 $ "//*-handout.pdf" %> \out -> do
            let src = dropSuffix "-handout.pdf" out ++ "-deck.md"
            meta <- getMeta
            markdownToPdfHandout src meta out

        priority 2 $ "//*-page.html" %> \out -> do
            let src = dropSuffix "-page.html" out ++ "-page.md"
            meta <- getMeta
            markdownToHtmlPage src meta out

        priority 2 $ "//*-page.pdf" %> \out -> do
            let src = dropSuffix "-page.pdf" out ++ "-page.md"
            meta <- getMeta
            markdownToPdfPage src meta out

        priority 2 $ "index.html" %> \out -> do
            exists <- Development.Shake.doesFileExist "index.md"
            let src = if exists then "index.md" else "index.md.generated"
            meta <- getMeta
            markdownToHtmlPage src meta out

        "index.md.generated" %> \out -> do
            decks <-getDecks
            handouts <- getHandouts
            pages <- getPages
            plain <- getPlain
            need $ decks ++ handouts ++ pages ++ plain
            writeIndex out decks handouts pages plain

        "//*.html" %> \out -> do
            let src = out -<.> "md"
            meta <- getMeta
            markdownToHtmlPage src meta out

        "//*.pdf" %> \out -> do
            let src = out -<.> "md"
            meta <- getMeta
            markdownToPdfPage src meta out

        phony "clean" $
            getEverything <++> getEverythingPdf <++> getCruft >>= removeFilesAfter "."

        phony "help" $
            liftIO $ B.putStr helpText

        phony "source" $ do
            source <- getAllSources
            meta <- getMeta
            liftIO $ mapM_ putStrLn $ source ++ meta

        phony "meta" $ do
            meta <- getMeta
            value <- readMetaData meta
            liftIO $ B.putStr $ encodePretty defConfig value

        phony "publish" $ do
            everything <- getEverything
            need everything
            hasResource <- Development.Shake.doesDirectoryExist resourceDir
            let source = if hasResource then resourceDir : everything else everything
            meta <- getMeta
            metaData <- readMetaData meta
            let host = metaValueAsString "rsync-destination.host" metaData
            let path = metaValueAsString "rsync-destination.path" metaData
            if isJust host && isJust path
               then do
                   cmd "ssh " (fromJust host) "mkdir -p" (fromJust path) :: Action ()
                   cmd "rsync -a" source $ intercalate ":" [fromJust host, fromJust path] :: Action ()
               else throw RsyncUrlException

        phony "cache" $ getAllSources >>= mapM_ cacheImages

        phony "clean-cache" $ do
            need ["clean"]
            removeFilesAfter "." ["**/cached"]

-- | The help page
helpText :: B.ByteString
helpText = $(makeRelativeToProject "resource/help-page.md" >>= embedFile)

-- | Glob for pathes below and relative to the current directory.
globRelative :: String -> Action [FilePath]
globRelative pat = liftIO $ glob pat >>= mapM makeRelativeToCurrentDirectory

-- | Some constants that might need tweaking
resourceDir = "img"
options = shakeOptions{shakeFiles=".shake"}
