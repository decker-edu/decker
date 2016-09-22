import Control.Exception
import Control.Monad ()
import qualified Data.ByteString.Char8 as B
import Data.IORef ()
import Data.List
import Data.Maybe
import Data.String ()
import Data.Yaml.Pretty
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Exit
import System.FilePath ()
import System.FilePath.Glob
import qualified Text.Mustache as M ()
import Text.Pandoc ()
import Text.Printf ()
import Utilities
import Context
import Embed

globA :: FilePattern -> Action [FilePath]
globA pattern =
  do projectDir <- getProjectDir
     liftIO $ globDir1 (compile pattern) projectDir

main :: IO ()
main = do
    -- Calculate some directories
    projectDir <- calcProjectDirectory
    let publicDir = projectDir </> "public"
    let cacheDir = publicDir </> "cache"
    let supportDir = publicDir </> "support"

    -- Find sources. These are formulated as actions in the Action mondad, such
    -- that each new iteration rescans all possible source files.
    let deckSourcesA = globA "**/*-deck.md"
    let pageSourcesA = globA "**/*-page.md"
    let allSourcesA = deckSourcesA <++> pageSourcesA

    let metaA = globA "**/*-meta.yaml"

    -- Read meta data.
    -- metaData <- readMetaDataIO meta

    -- Calculate targets
    let decksA = deckSourcesA >>= calcTargets ".md" ".html"
    let decksPdfA = deckSourcesA >>= calcTargets ".md" ".pdf"
    let handoutsA = deckSourcesA >>= calcTargets "-deck.md" "-handout.html"
    let handoutsPdfA = deckSourcesA >>= calcTargets "-deck.md" "-handout.pdf"
    let pagesA = pageSourcesA >>= calcTargets ".md" ".html"
    let pagesPdfA = pageSourcesA >>= calcTargets ".md" ".pdf"

    let indexSource = projectDir </> "index.md"
    let index = publicDir </> "index.html"
    let indexA = return [index] :: Action [FilePath]

    let everythingA = decksA <++> handoutsA <++> pagesA
    let everythingPdfA = decksPdfA <++> handoutsPdfA <++> pagesPdfA

    let cruft = map (combine projectDir) [ "index.md.generated"
                                         , "server.log"
                                         , "//.shake"
                                         ]

    context <- makeActionContext projectDir publicDir cacheDir supportDir
    runShakeInContext context options $ do

        want ["html"]

        phony "decks" $ do
            decksA >>= need

        phony "html" $ do
            everythingA <++> indexA >>= need

        phony "pdf" $ do
            pagesPdfA <++> handoutsPdfA <++> indexA >>= need

        phony "pdf-decks" $ do
            decksPdfA <++> indexA >>= need

        phony "watch" $ do
            need ["html"]
            allSourcesA <++> metaA >>= watchFiles

        phony "server" $ do
            need ["watch"]
            runHttpServer publicDir True

        phony "example" writeExampleProject

        priority 2 $ "//*-deck.html" %> \out -> do
            src <- calcSource "-deck.html" "-deck.md" out
            markdownToHtmlDeck src out

        priority 2 $ "//*-deck.pdf" %> \out -> do
            let src = replaceSuffix "-deck.pdf" "-deck.html" out
            runHttpServer publicDir False
            code <- cmd "decktape.sh reveal" ("http://localhost:8888/" ++ (makeRelative projectDir src)) out
            case code of
              ExitFailure _ -> do
                 throw $ DecktapeException "Unknown."
              ExitSuccess ->
                 return ()

        priority 2 $ "//*-handout.html" %> \out -> do
            src <- calcSource "-handout.html" "-deck.md" out
            markdownToHtmlHandout src out

        priority 2 $ "//*-handout.pdf" %> \out -> do
            src <- calcSource "-handout.pdf" "-deck.md" out
            markdownToPdfHandout src out

        priority 2 $ "//*-page.html" %> \out -> do
            src <- calcSource "-page.html" "-page.md" out
            markdownToHtmlPage src out

        priority 2 $ "//*-page.pdf" %> \out -> do
            src <- calcSource "-page.pdf" "-page.md" out
            markdownToPdfPage src out

        priority 2 $ index %> \out -> do
            exists <- Development.Shake.doesFileExist indexSource
            let src = if exists then indexSource else indexSource <.> "generated"
            markdownToHtmlPage src out

        indexSource <.> "generated" %> \out -> do
            decks <- decksA
            handouts <- handoutsA
            pages <- pagesA
            writeIndex out (takeDirectory index) decks handouts pages

        phony "clean" $ do
            removeFilesAfter publicDir ["//"]
            removeFilesAfter projectDir cruft

        phony "help" $
            liftIO $ putStr deckerHelpText

        phony "plan" $ do
            putNormal $ "project directory: " ++ projectDir
            putNormal $ "public directory: " ++ publicDir
            putNormal $ "support directory: " ++ supportDir
            putNormal "meta:"
            metaA >>= mapM_ putNormal
            putNormal "sources:"
            allSourcesA >>= mapM_ putNormal
            putNormal "targets:"
            everythingA <++> everythingPdfA >>= mapM_ putNormal

        phony "meta" $ do
            metaData <- metaA >>= readMetaData
            liftIO $ B.putStr $ encodePretty defConfig metaData

        phony "support" $ do
            writeEmbeddedFiles deckerSupportDir supportDir

        phony "publish" $ do
            everythingA <++> indexA >>= need
            metaData <- readMetaDataFor projectDir
            let host = metaValueAsString "rsync-destination.host" metaData
            let path = metaValueAsString "rsync-destination.path" metaData
            if isJust host && isJust path
               then do
                   cmd "ssh " (fromJust host) "mkdir -p" (fromJust path) :: Action ()
                   cmd "rsync -a" publicDir $ intercalate ":" [fromJust host, fromJust path] :: Action ()
               else throw RsyncUrlException

        phony "cache" $ do
            meta <- metaA
            sources <- allSourcesA
            cacheRemoteImages cacheDir meta sources

        phony "clean-cache" $ do
            need ["clean"]
            removeFilesAfter "." ["**/cached"]

-- | Some constants that might need tweaking
options = shakeOptions{shakeFiles=".shake"}

replaceSuffix srcSuffix targetSuffix filename = dropSuffix srcSuffix filename ++ targetSuffix

-- | Calculates the target pathes from a list of source files.
calcTargets :: String -> String -> [FilePath] -> Action [FilePath]
calcTargets srcSuffix targetSuffix sources =
  do projectDir <- getProjectDir
     publicDir <- getPublicDir
     return $ map (replaceSuffix srcSuffix targetSuffix . combine publicDir . makeRelative projectDir) sources

-- | Calculate the source file from the target path. Calls need.
calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target =
  do projectDir <- getProjectDir
     publicDir <- getPublicDir
     let src = (replaceSuffix targetSuffix srcSuffix . combine projectDir .  makeRelative publicDir) target
     need [src]
     return src
