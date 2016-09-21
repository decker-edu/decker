module Utilities
       (cacheRemoteImages, calcProjectDirectory, spawn, terminate,
        threadDelay', wantRepeat, waitForModificationIn, defaultContext,
        runShakeInContext, watchFiles, waitForTwitch, dropSuffix,
        stopServer, startServer, runHttpServer, writeIndex, readMetaData,
        readMetaDataIO, substituteMetaData, markdownToHtmlDeck,
        markdownToHtmlHandout, markdownToPdfHandout, markdownToHtmlPage,
        markdownToPdfPage, writeExampleProject, metaValueAsString, (<++>),
        markNeeded, replaceSuffixWith, writeEmbeddedFiles,
        getRelativeSupportDir, collectIncludes, pandocMakePdf,
        absoluteIncludePath, DeckerException(..))
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
import Embed

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

-- Runs liveroladx on the given directory, if it is not already running. If
-- open is True a browser window is opended.
runHttpServer dir open =
  do process <- getServerHandle
     case process of
       Just handle -> return ()
       Nothing ->
         do putNormal "# livereloadx (on http://localhost:8888, see server.log)"
            handle <-
              spawn $
              "livereloadx -s -p 8888 -d 500 " ++ dir ++ " 2>&1 > server.log"
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
writeIndex out baseUrl decks handouts pages =
  do let decksLinks = map (makeRelative baseUrl) decks
     let handoutsLinks = map (makeRelative baseUrl) handouts
     let pagesLinks = map (makeRelative baseUrl) pages
     liftIO $
       writeFile out $
       unlines ["---"
               ,"title: Generated Index"
               , "subtitle: {{course}} ({{semester}})"
               ,"---"
               ,"# Slide decks"
               ,unlines $ map makeLink $ sort decksLinks
               ,"# Handouts"
               ,unlines $ map makeLink $ sort handoutsLinks
               ,"# Supporting Documents"
               ,unlines $ map makeLink $ sort pagesLinks]
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
  :: FilePath -> MT.Value -> IO T.Text
substituteMetaData source metaData =
  do result <-  M.localAutomaticCompile source
     case result of
       Right template -> return $ M.substituteValue template metaData
       Left err -> throw $ MustacheException (show err)

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from =
  do supportDir <- getSupportDir
     publicDir <- getPublicDir
     return $
       invertPath
         (makeRelative publicDir
                       (takeDirectory from)) </>
       (makeRelative publicDir supportDir)
  where invertPath fp = joinPath $ map (\_ -> "..") $ filter ((/=) ".") $ splitPath fp

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck
  :: FilePath -> MetaData -> FilePath -> Action ()
markdownToHtmlDeck markdownFile metaData out =
  do supportDir <- getRelativeSupportDir out
     let options =
           def {writerStandalone = True
               ,writerTemplate = deckTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (supportDir </> "katex/katex.min.js")
                        (supportDir </> "katex/katex.min.css")
               ,writerVariables =
                  [("revealjs-url",supportDir </> "reveal.js")]
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown metaData markdownFile
     processed <- processPandocDeck "revealjs" pandoc
     let images = extractLocalImagePathes processed
     copyLocalImages images markdownFile out
     writePandocString "revealjs" options out processed

copyLocalImages :: [FilePath] -> FilePath  -> FilePath -> Action ()
copyLocalImages imageFiles inFile outFile =
  do let inDir = takeDirectory inFile
     let outDir = takeDirectory outFile
     mapM_ (copyImageFile inDir outDir) imageFiles
  where copyImageFile inDir outDir imageFile =
          do let from = inDir </> imageFile
             let to = outDir </> imageFile
             liftIO $ createDirectoryIfMissing True (takeDirectory to)
             copyFileChanged from to

type MetaData = Y.Value

-- | Selects a matching pandoc string writer for the format string, or throws an
-- exception.
getPandocWriter :: String -> StringWriter
getPandocWriter format =
  case getWriter format of
    Right (PureStringWriter w) -> w
    Left e -> throw $ PandocException e
    _ -> throw $ PandocException $ "No writer for format: " ++ format

-- | Reads a markdownfile, expands the included files, and substitutes mustache
-- template variables.
readAndPreprocessMarkdown :: MetaData -> FilePath -> Action Pandoc
readAndPreprocessMarkdown metaData markdownFile =
  do -- let writer = getPandocWriter format
     projectDir <- getProjectDir
     let baseDir = takeDirectory markdownFile
     includes <- collectIncludes markdownFile
     pandoc <- readMetaMarkdown markdownFile metaData
     need includes
     liftIO $ processIncludes projectDir baseDir metaData pandoc

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage
  :: FilePath -> MetaData -> FilePath -> Action ()
markdownToHtmlPage markdownFile metaData out =
  do supportDir <- getRelativeSupportDir out
     let options =
           def {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = pageTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (supportDir </> "katex/katex.min.js")
                        (supportDir </> "katex/katex.min.css")
               ,writerVariables =
                  [("css",supportDir </> "readable/bootstrap.min.css")]
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown metaData markdownFile
     processed <- processPandocPage "html5" pandoc
     let images = extractLocalImagePathes processed
     copyLocalImages images markdownFile out
     writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage
  :: FilePath -> MetaData -> FilePath -> Action ()
markdownToPdfPage markdownFile metaData out =
  do let options =
           def {writerStandalone = True
               ,writerTemplate = pageLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown metaData markdownFile
     processed <- processPandocPage "latex" pandoc
     putNormal $ "# pandoc (for " ++ out ++ ")"
     pandocMakePdf options processed out

pandocMakePdf options processed out =
  do result <- liftIO $ makePDF "pdflatex" writeLaTeX options processed
     case result of
       Left err -> throw $ PandocException (show err)
       Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToHtmlHandout markdownFile metaFiles out =
  do metaData <- readMetaData metaFiles
     pandoc <- readMetaMarkdown markdownFile metaData
     processed <- processPandocHandout "html" pandoc
     supportDir <- getRelativeSupportDir out
     let options =
           def {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = handoutTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod =
                  KaTeX (supportDir </> "katex/katex.min.js")
                        (supportDir </> "katex/katex.min.css")
               ,writerVariables =
                  [("css",supportDir </> "readable/bootstrap.min.css")]
               ,writerCiteMethod = Citeproc}
     writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout
  :: FilePath -> [FilePath] -> FilePath -> Action ()
markdownToPdfHandout markdownFile metaFiles out =
  do metaData <- readMetaData metaFiles
     pandoc <- readMetaMarkdown markdownFile metaData
     processed <- processPandocHandout "latex" pandoc
     let options =
           def {writerStandalone = True
               ,writerTemplate = handoutLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     putNormal $ "# pandoc (for " ++ out ++ ")"
     pandocMakePdf options processed out

readMetaMarkdown
  :: FilePath -> Y.Value -> Action Pandoc
readMetaMarkdown markdownFile metaData = liftIO $ readMetaMarkdownIO markdownFile metaData

readMetaMarkdownIO
  :: FilePath -> Y.Value -> IO Pandoc
readMetaMarkdownIO markdownFile metaData =
  do text <-
       substituteMetaData markdownFile
                          (MT.mFromJSON metaData)
     case readMarkdown def $ T.unpack text of
       Right pandoc -> return pandoc
       Left err -> throw $ PandocException (show err)

cacheRemoteImages :: FilePath -> [FilePath] -> [FilePath] -> Action ()
cacheRemoteImages cacheDir metaFiles markdownFiles =
  do mapM_ cacheImages markdownFiles
  where cacheImages markdownFile =
          do metaData <- readMetaData metaFiles
             pandoc <- readMetaMarkdown markdownFile metaData
             _ <- liftIO $ walkM (cachePandocImages cacheDir) pandoc
             putNormal $ "# pandoc (cached images for " ++ markdownFile ++ ")"

absoluteIncludePath root base path =
  if isAbsolute path
     then root </> makeRelative "/" path
     else base </> path

-- Transitively collects all include files for the given markdown file. The returned pathes
-- are absolute an can be passed directly to `need`.
collectIncludes ::  FilePath -> Action [FilePath]
collectIncludes markdownFile =
  do projectDir <- getProjectDir
     liftIO $ collectIncludesIO projectDir markdownFile

collectIncludesIO :: FilePath -> FilePath -> IO [FilePath]
collectIncludesIO rootDir markdownFile =
  do markdown <- readFile markdownFile
     let Pandoc _ blocks =
           case readMarkdown def markdown of
             Right p -> p
             Left e -> throw $ PandocException (show e)
     let baseDir = takeDirectory markdownFile
     let direct = map (absoluteIncludePath rootDir baseDir) (foldl include [] blocks)
     transitive <- mapM (collectIncludesIO rootDir) direct
     return $ direct ++ concat transitive
  where include :: [FilePath] -> Block -> [FilePath]
        include result (Para [Image _ [Str "#include"] (url,_)]) = url : result
        include result _ = result

-- Transitively splices all include files into the pandoc document.
processIncludes :: FilePath -> FilePath -> Y.Value -> Pandoc -> IO Pandoc
processIncludes rootDir baseDir metaData (Pandoc meta blocks) =
  do included <- processBlocks baseDir blocks
     return $ Pandoc meta included
  where processBlocks
          :: FilePath -> [Block] -> IO [Block]
        processBlocks base blcks =
          do spliced <- foldM (include base) [] blcks
             return $ concat $ reverse spliced
        include
          :: FilePath -> [[Block]] -> Block -> IO [[Block]]
        include base result (Para [Image _ [Str "#include"] (url,_)]) =
          do let filePath = absoluteIncludePath rootDir base url
             Pandoc _ b <- readMetaMarkdownIO filePath metaData
             included <-
               processBlocks (takeDirectory filePath)
                             b
             return $ included : result
        include _ result block = return $ [block] : result

processPandocPage
  :: String -> Pandoc -> Action Pandoc
processPandocPage format pandoc =
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

writePandocString :: String
                  -> WriterOptions
                  -> FilePath
                  -> Pandoc
                  -> Action ()
writePandocString format options out pandoc =
  do let writer = getPandocWriter format
     writeFile' out
                (writer options pandoc)
     putNormal $ "# pandoc for (" ++ out ++ ")"

writeExampleProject :: Action ()
writeExampleProject = mapM_ writeOne deckerExampleDir
  where writeOne (path,contents) =
          do exists <- Development.Shake.doesFileExist path
             unless exists $
               do liftIO $ createDirectoryIfMissing True (takeDirectory path)
                  liftIO $ B.writeFile path contents
                  putNormal $ "# create (for " ++ path ++ ")"

writeEmbeddedFiles :: [(FilePath, B.ByteString)] -> FilePath -> Action ()
writeEmbeddedFiles files dir =
  do let absolute = map (\(path,contents) -> (dir </> path,contents)) files
     mapM_ write absolute
  where write (path,contents) =
          do liftIO $
               Dir.createDirectoryIfMissing True
                                            (takeDirectory path)
             exists <- liftIO $ Dir.doesFileExist path
             when (not exists) $ liftIO $ B.writeFile path contents

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
  | HttpException String
  | RsyncUrlException
  | DecktapeException String
  deriving Typeable

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) =
    "decktape.sh failed for reason: " ++ e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
