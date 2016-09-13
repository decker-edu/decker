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

main :: IO ()
main = do
    -- Calculate some directories
    projectDir <- calcProjectDirectory
    let publicDir = projectDir </> publicDirName
    let cacheDir = publicDir </> "cache"
    let supportDir = publicDir </> "support"

    -- Find sources
    deckSources <- glob "**/*-deck.md"
    pageSources <- glob "**/*-page.md"
    allSources <- glob "**/*.md"
    meta <- glob "**/*.yaml"

    -- let plainSources = allSources \\ (deckSources ++ pageSources)

    -- Calculate targets
    let decks = targetPathes deckSources projectDir ".md" ".html"
    let decksPdf = targetPathes deckSources projectDir ".md" ".pdf"
    let handouts = targetPathes deckSources projectDir "-deck.md" "-handout.html"
    let handoutsPdf = targetPathes deckSources projectDir "-deck.md" "-handout.pdf"
    let pages = targetPathes pageSources projectDir ".md" ".html"
    let pagesPdf = targetPathes pageSources projectDir ".md" ".pdf"
    -- let plain = targetPathes plainSources projectDir ".md" ".html"
    -- let plainPdf = targetPathes pageSources projectDir ".md" ".pdf"

    let indexSource = projectDir </> "index.md"
    let index = publicDir </> "index.html"

    let everything = decks ++ handouts ++ pages ++ [index]
    let everythingPdf = decksPdf ++ handoutsPdf ++ pagesPdf

    let cruft = [ "index.md.generated"
                , "server.log"
                , "//.shake"
                ]

    context <- makeActionContext projectDir publicDir cacheDir supportDir
    runShakeInContext context options $ do

        want ["html"]

        phony "decks" $ do
            need decks

        phony "html" $ do
            need $ everything ++ [index]

        phony "pdf" $ do
            need $ pagesPdf ++ handoutsPdf ++ [index]

        phony "pdf-decks" $ do
            need $ decksPdf ++ [index]

        phony "watch" $ do
            need ["html"]
            watchFiles $ allSources ++ meta

        phony "server" $ do
            need ["watch"]
            runHttpServer publicDir True

        phony "example" writeExampleProject

        priority 2 $ "//*-deck.html" %> \out -> do
            need ["support"]
            let src = sourcePath out projectDir ".html" ".md"
            markdownToHtmlDeck src meta out

        priority 2 $ "//*-deck.pdf" %> \out -> do
            let src = sourcePath out projectDir ".pdf" ".html"
            need [src]
            runHttpServer publicDir False
            code <- cmd "decktape.sh reveal" ("http://localhost:8888/" ++ src) out
            case code of
              ExitFailure _ -> do
                 throw $ DecktapeException "Unknown."
              ExitSuccess ->
                 return ()

        priority 2 $ "//*-handout.html" %> \out -> do
            need ["support"]
            let src = sourcePath out projectDir "-handout.html" "-deck.md"
            markdownToHtmlHandout src meta out

        priority 2 $ "//*-handout.pdf" %> \out -> do
            let src = sourcePath out projectDir "-handout.pdf" "-deck.md"
            markdownToPdfHandout src meta out

        priority 2 $ "//*-page.html" %> \out -> do
            need ["support"]
            let src = sourcePath out projectDir "-page.html" "-page.md"
            markdownToHtmlPage src meta out

        priority 2 $ "//*-page.pdf" %> \out -> do
            let src = sourcePath out projectDir "-page.pdf" "-page.md"
            markdownToPdfPage src meta out

        priority 2 $ index %> \out -> do
            exists <- Development.Shake.doesFileExist indexSource
            let src = if exists then indexSource else indexSource <.> "generated"
            putNormal out
            rel <- getRelativeSupportDir out
            putNormal rel
            markdownToHtmlPage src meta out

        indexSource <.> "generated" %> \out -> do
            need $ decks ++ handouts ++ pages
            writeIndex out (takeDirectory index) decks handouts pages

        "//*.html" %> \out -> do
            let src = out -<.> "md"
            markdownToHtmlPage src meta out

        "//*.pdf" %> \out -> do
            let src = out -<.> "md"
            markdownToPdfPage src meta out

        phony "clean" $ do
            removeFilesAfter publicDir ["//"]
            removeFilesAfter projectDir cruft

        phony "help" $
            liftIO $ putStr deckerHelpText

        phony "plan" $ do
            putNormal $ "project directory: " ++ projectDir
            putNormal "sources:"
            mapM_ putNormal $ allSources ++ meta
            putNormal "targets:"
            mapM_ putNormal $ everything ++ everythingPdf

        phony "meta" $ do
            value <- readMetaData meta
            liftIO $ B.putStr $ encodePretty defConfig value

        phony "support" $ do
            writeEmbeddedFiles deckerSupportDir supportDir

        phony "publish" $ do
            need $ everything ++ ["index.html"]
            hasResource <- Development.Shake.doesDirectoryExist resourceDir
            let source = if hasResource then resourceDir : everything else everything
            metaData <- readMetaData meta
            let host = metaValueAsString "rsync-destination.host" metaData
            let path = metaValueAsString "rsync-destination.path" metaData
            if isJust host && isJust path
               then do
                   cmd "ssh " (fromJust host) "mkdir -p" (fromJust path) :: Action ()
                   cmd "rsync -a" source $ intercalate ":" [fromJust host, fromJust path] :: Action ()
               else throw RsyncUrlException

        phony "cache" $
            cacheRemoteImages cacheDir meta allSources

        phony "clean-cache" $ do
            need ["clean"]
            removeFilesAfter "." ["**/cached"]

        phony "self-test" $ do
          ctx <- getActionContext
          putNormal $ show ctx

-- | Glob for pathes below and relative to the current directory.
globRelative :: String -> Action [FilePath]
globRelative pat = liftIO $ glob pat >>= mapM makeRelativeToCurrentDirectory

-- | Glob for pathes below and relative to the current directory.
globRelativeIO :: String -> IO [FilePath]
globRelativeIO pat = glob pat >>= mapM makeRelativeToCurrentDirectory

-- | Some constants that might need tweaking
resourceDir = "img"
options = shakeOptions{shakeFiles=".shake"}

publicDirName :: String
publicDirName = "public"

targetPath :: FilePath -> FilePath -> String -> String -> FilePath
targetPath source projectDir srcSuffix targetSuffix =
  let target = projectDir </> publicDirName </> (makeRelative projectDir source)
  in dropSuffix srcSuffix target ++ targetSuffix

targetPathes :: [FilePath] -> FilePath -> String -> String -> [FilePath]
targetPathes sources projectDir srcSuffix targetSuffix =
  [targetPath s projectDir srcSuffix targetSuffix | s <- sources]

sourcePath :: FilePath -> FilePath -> String -> String -> FilePath
sourcePath out projectDir targetSuffix srcSuffix =
  let source = projectDir </> (makeRelative (projectDir </> publicDirName) out)
  in dropSuffix targetSuffix source ++ srcSuffix
