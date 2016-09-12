{-# LANGUAGE TemplateHaskell #-}

module Utilities
       (cacheRemoteImages, calcProjectDirectory, helpText, spawn,
        terminate, threadDelay', wantRepeat, waitForModificationIn,
        defaultContext, runShakeInContext, watchFiles,
        waitForTwitch, dropSuffix, stopServer, startServer, runHttpServer,
        writeIndex, readMetaData, readMetaDataIO, substituteMetaData,
        markdownToHtmlDeck, markdownToHtmlHandout, markdownToPdfHandout,
        markdownToHtmlPage, markdownToPdfPage, getBaseUrl,
        writeExampleProject, metaValueAsString, (<++>), markNeeded,
        replaceSuffixWith, DeckerException(..))
       where

import Control.Monad.Loops
import Control.Monad
import Control.Concurrent
import Control.Exception
import Development.Shake
import Development.Shake.FilePath
import Data.Dynamic
import Data.List.Extra
import Data.Maybe
import Data.FileEmbed
import Data.IORef
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.HashMap.Lazy as HashMap
import Text.Printf
import System.Process
import System.Process.Internals
import System.Directory as Dir
import System.Exit
import System.Posix.Signals
import System.FilePath
import qualified Data.Yaml as Y
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.PDF
import Text.CSL.Pandoc
import Filter
import Debug.Trace
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.URI
import Text.Highlighting.Kate.Styles
import Context

-- Find the project directory and change current directory to there. The project directory is the first upwards directory that contains a .git directory entry.
calcProjectDirectory :: IO FilePath
calcProjectDirectory =
  do cwd <- getCurrentDirectory
     pd <- searchGitRoot cwd
     return pd
  where searchGitRoot :: FilePath -> IO FilePath
        searchGitRoot path =
          do if isDrive path
                then makeAbsolute "."
                else do hasGit <- Dir.doesDirectoryExist (path </> ".git")
                        if hasGit
                           then makeAbsolute path
                           else searchGitRoot $ takeDirectory path

-- Utility functions for shake based apps
spawn :: String -> Action ProcessHandle
spawn = liftIO . spawnCommand

-- Runs liveroladx on the current directory, if it is not already running. If
-- open is True a browser window is opended.
runHttpServer open =
  do baseUrl <- getBaseUrl
     process <- getServerHandle
     case process of
       Just handle -> return ()
       Nothing ->
         do putNormal "# livereloadx (on http://localhost:8888, see server.log)"
            putNormal ("#     DECKER_RESOURCE_BASE_URL=" ++ baseUrl)
            handle <-
              liftIO $
              spawnCommand "livereloadx -s -p 8888 -d 500 2>&1 > server.log"
            setServerHandle $ Just handle
            threadDelay' 200000
            when open $ cmd "open http://localhost:8888/" :: Action ()

startServer id command =
  liftIO $
  do processHandle <- spawnCommand command
     withProcessHandle processHandle handleResult
  where handleResult ph =
          case ph of
            ClosedHandle e ->
              print $ "Error starting server " ++ id ++ ": " ++ show e
            OpenHandle p ->
              do print $ "Server " ++ id ++ " running (" ++ show p ++ ")"
                 writeFile (id ++ ".pid")
                           (show p)

stopServer id =
  liftIO $
  do let pidFile = id ++ ".pid"
     result <- try $ readFile pidFile
     case result of
       Left (SomeException e) -> print $ "Unable to read file " ++ pidFile
       Right pid ->
         do exitCode <- system ("kill -9 " ++ pid)
            removeFile pidFile

terminate :: ProcessHandle -> Action ()
terminate = liftIO . terminateProcess

threadDelay' :: Int -> Action ()
threadDelay' = liftIO . threadDelay

wantRepeat :: IORef Bool -> Action ()
wantRepeat justOnce = liftIO $ writeIORef justOnce False

waitForModificationIn :: [FilePath] -> Action ()
waitForModificationIn = liftIO . waitForTwitch

-- The context of program invocation consists of a list of
-- files to watch and a possibly running local http server.
data Context =
  Context [FilePath]
          (Maybe ProcessHandle)

defaultContext = Context [] Nothing

runShakeInContext :: ActionContext -> ShakeOptions -> Rules () -> IO ()
runShakeInContext context options rules =
  do opts <- setActionContext context options
     tid <- myThreadId
     installHandler keyboardSignal
                    (Catch (cleanup tid))
                    Nothing
     untilM_ (tryRunShake opts) nothingToWatch
     cleanup tid
  where tryRunShake opts =
          do catch (shakeArgs opts rules)
                   (\(SomeException e) -> return ())
        cleanup tid =
          do process <- readIORef $ ctxServerHandle context
             case process of
               Just handle -> terminateProcess handle
               Nothing -> return ()
             throwTo tid ExitSuccess
        nothingToWatch =
          do files <- readIORef $ ctxFilesToWatch context
             if null files
                then return True
                else do waitForTwitch files
                        return False

watchFiles files = setFilesToWatch files


-- | Actively waits for the first change to any member in the set of specified
-- | files and their parent directories, then returns.
waitForTwitch files =
  do startTime <- getCurrentTime
     let dirs = map takeDirectory files
     let filesAndDirs = Set.toList . Set.fromList $ files ++ dirs
     whileM_ (noModificationSince startTime filesAndDirs)
             (threadDelay 300000)
  where noModificationSince startTime pathes =
          do modified <- mapM (modifiedSince startTime) pathes
             return $ not (or modified)
        modifiedSince time path =
          handle (\(SomeException _) -> return False) $
          do modTime <- getModificationTime path
             return $ diffUTCTime modTime time > 0

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

-- | Mark files as need and return them
markNeeded :: [FilePath] -> Action [FilePath]
markNeeded files =
  do need files
     return files

-- | Removes the last suffix from a filename
dropSuffix s t = fromMaybe t (stripSuffix s t)

-- | Monadic version of suffix replacement for easy binding.
replaceSuffixWith
  :: String -> String -> [FilePath] -> Action [FilePath]
replaceSuffixWith suffix with pathes =
  return [dropSuffix suffix d ++ with | d <- pathes]

-- | Monadic version of suffix replacement for easy binding.
calcTargetPath
  :: FilePath -> String -> String -> [FilePath] -> Action [FilePath]
calcTargetPath projectDir suffix with pathes =
  return [projectDir </> dropSuffix suffix d ++ with | d <- pathes]

-- | Generates an index.md file with links to all generated files of interest.
writeIndex out baseUrl decks handouts pages plain =
  do let decksLinks = map (makeRelative baseUrl) decks
     let handoutsLinks = map (makeRelative baseUrl) handouts
     let pagesLinks = map (makeRelative baseUrl) pages
     let plainLinks = map (makeRelative baseUrl) plain
     liftIO $
       writeFile out $
       unlines ["# Index"
               ,"## Slide decks"
               ,unlines $ map makeLink decksLinks
               ,"## Handouts"
               ,unlines $ map makeLink handoutsLinks
               ,"## Supporting Documents"
               ,unlines $ map makeLink pagesLinks
               ,"## Everything else"
               ,unlines $ map makeLink plainLinks]
  where makeLink path = "-    [" ++ takeFileName path ++ "](" ++ path ++ ")"

-- | Decodes an array of YAML files and combines the data into one object.
-- Key value pairs from later files overwrite those from early ones.
readMetaDataIO :: [FilePath] -> IO Y.Value
readMetaDataIO files =
  mapM decode files >>= foldM combine (Y.Object HashMap.empty)
  where decode file =
          do result <- Y.decodeFileEither file
             return (file,result)
        combine (Y.Object obj) (file,Right (Y.Object new)) =
          return (Y.Object (HashMap.union new obj))
        combine obj (file,Right _) =
          do throw $
               YamlException $
               file ++ ": top level metadata is not a YAML object."
             return obj
        combine obj (file,Left err) =
          do throw $
               YamlException $ file ++ ": " ++ Y.prettyPrintParseException err
             return obj

readMetaData files = liftIO $ readMetaDataIO files

-- | Substitutes meta data values in the provided file.
substituteMetaData
  :: FilePath -> MT.Value -> Action T.Text
substituteMetaData source metaData =
  do result <- liftIO $ M.localAutomaticCompile source
     case result of
       Right template -> return $ M.substituteValue template metaData
       Left err -> throw $ MustacheException (show err)

getBaseUrl =
  getEnvWithDefault "https://tramberend.beuth-hochschule.de/cdn/" "DECKER_RESOURCE_BASE_URL"

-- | The help page
helpText :: B.ByteString
helpText = $(makeRelativeToProject "resource/help-page.md" >>= embedFile)

deckTemplate :: String
deckTemplate =
  B.unpack $(makeRelativeToProject "resource/deck.html" >>= embedFile)

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToHtmlDeck markdownFile metaFiles out =
  do need $ markdownFile : metaFiles
     pandoc <- readMetaMarkdown markdownFile metaFiles
     processed <- processPandocDeck "revealjs" pandoc
     let images = extractLocalImagePathes pandoc
     putNormal $ "images: " ++ show images
     baseUrl <- getBaseUrl
     let options =
           def {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = deckTemplate
               ,writerSlideVariant = RevealJsSlides
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (baseUrl ++ "katex-0.6.0/katex.min.js")
                        (baseUrl ++ "katex-0.6.0/katex.min.css")
               ,writerVariables = [("revealjs-url",baseUrl ++ "reveal.js")]
               ,writerCiteMethod = Citeproc}
     writePandocString writeHtmlString options out processed

pageTemplate :: String
pageTemplate =
  B.unpack $(makeRelativeToProject "resource/page.html" >>= embedFile)

pageLatexTemplate :: String
pageLatexTemplate =
  B.unpack $(makeRelativeToProject "resource/page.tex" >>= embedFile)

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToHtmlPage markdownFile metaFiles out =
  do need $ markdownFile : metaFiles
     pandoc <- readMetaMarkdown markdownFile metaFiles
     processed <- processPandocDeck "html" pandoc
     baseUrl <- getBaseUrl
     let options =
           def {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = pageTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (baseUrl ++ "katex-0.6.0/katex.min.js")
                        (baseUrl ++ "katex-0.6.0/katex.min.css")
               ,writerCiteMethod = Citeproc}
     writePandocString writeHtmlString options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToPdfPage markdownFile metaFiles out =
  do need $ markdownFile : metaFiles
     pandoc <- readMetaMarkdown markdownFile metaFiles
     processed <- processPandoc "latex" pandoc
     baseUrl <- getBaseUrl
     let options =
           def {writerStandalone = True
               ,writerTemplate = pageLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     putNormal $ "# pandoc (for " ++ out ++ ")"
     pandocMakePdf options processed out

pandocMakePdf options processed out =
  do result <- liftIO $ makePDF "pdflatex" writeLaTeX options processed
     case result of
       Left err -> throw $ PandocException (show err)
       Right pdf -> liftIO $ LB.writeFile out pdf

handoutTemplate :: String
handoutTemplate =
  B.unpack $(makeRelativeToProject "resource/handout.html" >>= embedFile)

handoutLatexTemplate :: String
handoutLatexTemplate =
  B.unpack $(makeRelativeToProject "resource/handout.tex" >>= embedFile)

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToHtmlHandout markdownFile metaFiles out =
  do need $ markdownFile : metaFiles
     pandoc <- readMetaMarkdown markdownFile metaFiles
     processed <- processPandocHandout "html" pandoc
     baseUrl <- getBaseUrl
     let options =
           def {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = handoutTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (baseUrl ++ "katex-0.6.0/katex.min.js")
                        (baseUrl ++ "katex-0.6.0/katex.min.css")
               ,writerCiteMethod = Citeproc}
     writePandocString writeHtmlString options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToPdfHandout markdownFile metaFiles out =
  do need $ markdownFile : metaFiles
     pandoc <- readMetaMarkdown markdownFile metaFiles
     processed <- processPandocHandout "latex" pandoc
     baseUrl <- getBaseUrl
     let options =
           def {writerStandalone = True
               ,writerTemplate = handoutLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     putNormal $ "# pandoc (for " ++ out ++ ")"
     pandocMakePdf options processed out

readMetaMarkdown
  :: FilePath -> [FilePath] -> Action Pandoc
readMetaMarkdown markdownFile metaFiles =
  do metaData <- readMetaData metaFiles
     text <-
       substituteMetaData markdownFile
                          (MT.mFromJSON metaData)
     case readMarkdown def $ T.unpack text of
       Right pandoc -> return pandoc
       Left err -> throw $ PandocException (show err)

cacheRemoteImages :: FilePath -> [FilePath] -> [FilePath] -> Action ()
cacheRemoteImages cacheDir metaFiles markdownFiles =
  do mapM_ cacheImages markdownFiles
  where cacheImages markdownFile =
          do pandoc <- readMetaMarkdown markdownFile metaFiles
             liftIO $ walkM (cachePandocImages cacheDir) pandoc
             putNormal $ "# pandoc (cached images for " ++ markdownFile ++ ")"

processPandoc
  :: String -> Pandoc -> Action Pandoc
processPandoc format pandoc =
  do let f = Just (Format format)
     -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
     cacheDir <- getCacheDir
     processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
     return $ expandMacros f processed

processPandocDeck
  :: String -> Pandoc -> Action Pandoc
processPandocDeck format pandoc =
  do let f = Just (Format format)
     -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
     cacheDir <- getCacheDir
     processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
     return $ (makeSlides f . expandMacros f) processed

processPandocHandout
  :: String -> Pandoc -> Action Pandoc
processPandocHandout format pandoc =
  do let f = Just (Format format)
     -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
     cacheDir <- getCacheDir
     processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
     return $ (expandMacros f . filterNotes f) processed

type StringWriter = WriterOptions -> Pandoc -> String

writePandocString :: StringWriter
                  -> WriterOptions
                  -> FilePath
                  -> Pandoc
                  -> Action ()
writePandocString writer options out pandoc =
  do writeFile' out
                (writer options pandoc)
     putNormal $ "# pandoc for (" ++ out ++ ")"

writeExampleProject :: Action ()
writeExampleProject = mapM_ writeOne deckerExampleFiles
  where deckerExampleFiles =
          [("example-deck.md"
           ,B.unpack $(makeRelativeToProject "resource/example/example-deck.md" >>=
                       embedFile))
          ,("example-meta.yaml"
           ,B.unpack $(makeRelativeToProject "resource/example/example-meta.yaml" >>=
                       embedFile))
          ,("example-page.md"
           ,B.unpack $(makeRelativeToProject "resource/example/example-page.md" >>=
                       embedFile))]
        writeOne (path,contents) =
          do exists <- Development.Shake.doesFileExist path
             unless exists $
               do writeFile' path contents
                  putNormal $ "# create (for " ++ path ++ ")"

lookupValue :: String -> Y.Value -> Maybe Y.Value
lookupValue key (Y.Object hashTable) =
  HashMap.lookup (T.pack key)
                 hashTable
lookupValue key _ = Nothing

metaValueAsString
  :: String -> Y.Value -> Maybe String
metaValueAsString key meta =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (lookupValue k meta)
  where lookup'
          :: [String] -> Maybe Y.Value -> Maybe String
        lookup' [] (Just (Y.String text)) = Just (T.unpack text)
        lookup' [] (Just (Y.Number n)) = Just (show n)
        lookup' [] (Just (Y.Bool b)) = Just (show b)
        lookup' (k:ks) (Just obj@(Y.Object _)) = lookup' ks (lookupValue k obj)
        lookup' _ _ = Nothing

-- | Tool specific exceptions
data DeckerException
  = MustacheException String
  | PandocException String
  | YamlException String
  | RsyncUrlException
  | DecktapeException String
  deriving (((Typeable)))

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException cdn) =
    "decktape.sh failed. Is environment varible 'DECKER_RESOURCE_BASE_URL' set correctly (currently " ++
    cdn ++ ")?"
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
