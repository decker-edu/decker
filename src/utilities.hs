module Utilities
       (calcProjectDirectory, spawn, terminate, threadDelay', wantRepeat,
        waitForModificationIn, defaultContext, runShakeInContext,
        watchFiles, waitForTwitch, dropSuffix, stopServer, startServer,
        runHttpServer, writeIndex, readMetaData, readMetaDataForDir,
        readMetaDataIO, substituteMetaData, markdownToHtmlDeck,
        markdownToHtmlHandout, markdownToPdfHandout, markdownToHtmlPage,
        markdownToPdfPage, writeExampleProject, metaValueAsString, (<++>),
        markNeeded, replaceSuffixWith, writeEmbeddedFiles,
        getRelativeSupportDir, pandocMakePdf, isCacheableURI,
        adjustLocalUrl, cacheRemoteFile, cacheRemoteImages, makeRelativeTo, fixMustacheMarkup, fixMustacheMarkupText,
        DeckerException(..))
       where

import Control.Monad.Loops
import Control.Monad
import Control.Concurrent
import Control.Exception
import Development.Shake
import Development.Shake.FilePath as SFP
import Data.Dynamic
import Data.List.Extra
import Data.Maybe
import Data.IORef
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time.Clock
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.HashMap.Lazy as HashMap
import Text.Printf
import System.Process
import System.Process.Internals
import System.Directory as Dir
import System.Exit
import System.FilePath as SF
import System.FilePath.Glob
import qualified Data.Yaml as Y
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Digest.Pure.MD5
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Pandoc.PDF
import Text.CSL.Pandoc
import Filter
import Debug.Trace
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI
import Text.Highlighting.Kate.Styles
import Context
import Embed

-- Find the project directory and change current directory to there. The project directory is the first upwards directory that contains a .git directory entry.
calcProjectDirectory :: IO FilePath
calcProjectDirectory =
  do cwd <- getCurrentDirectory
     searchGitRoot cwd
  where searchGitRoot :: FilePath -> IO FilePath
        searchGitRoot path =
          if isDrive path
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
     catch (untilM_ (tryRunShake opts) nothingToWatch)
           (\(SomeException e) -> putStrLn $ "Terminated: " ++ show e)
     cleanup
  where tryRunShake opts =
          catch (shakeArgs opts rules)
                   (\(SomeException e) -> return ())
        cleanup =
          do process <- readIORef $ ctxServerHandle context
             case process of
               Just handle -> terminateProcess handle
               Nothing -> return ()
        nothingToWatch =
          do files <- readIORef $ ctxFilesToWatch context
             if null files
                then return True
                else do waitForTwitch files
                        return False

watchFiles = setFilesToWatch


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
     projectDir <- getProjectDir
     liftIO $
       writeFile out $
       unlines ["---"
               ,"title: Generated Index"
               ,"subtitle: " ++ projectDir
               ,"---"
               ,"# Slide decks"
               ,unlines $ map makeLink $ sort decksLinks
               ,"# Handouts"
               ,unlines $ map makeLink $ sort handoutsLinks
               ,"# Supporting Documents"
               ,unlines $ map makeLink $ sort pagesLinks]
  where makeLink path = "-    [" ++ takeFileName path ++ "](" ++ path ++ ")"

readMetaDataForDir ::  FilePath -> Action Y.Value
readMetaDataForDir dir = walkUpTo dir
  where
    walkUpTo dir = do
        projectDir <- getProjectDir
        if equalFilePath projectDir dir
            then collectMeta dir
            else do
                fromAbove <- walkUpTo (takeDirectory dir)
                fromHere <- collectMeta dir
                return $ joinMeta fromHere fromAbove
    --
    collectMeta dir = do
        files <- liftIO $ globDir1 (compile "*-meta.yaml") dir
        need files
        meta <- mapM decodeYaml files
        return $ foldl joinMeta (Y.object []) meta
    --
    joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
    joinMeta _ _ = throw $ YamlException "Can only join YAML objects."
    --
    decodeYaml yamlFile = do
        result <- liftIO $ Y.decodeFileEither yamlFile
        case result of
            Right object@(Y.Object _) -> return object
            Right _ ->
                throw $
                YamlException $
                "Top-level meta value must be an object: " ++ dir
            Left exception -> throw exception

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
          do _ <- throw $
               YamlException $
               file ++ ": top level metadata is not a YAML object."
             return obj
        combine obj (file,Left err) =
          do _ <- throw $
               YamlException $ file ++ ": " ++ Y.prettyPrintParseException err
             return obj

-- | TODO This has to be restructured. Metadata files need to be calculated from
-- the source directory and need should be called implicitly.
readMetaData :: [FilePath] -> Action MetaData
readMetaData files =
  do need files
     liftIO $ readMetaDataIO files

-- | Fixes pandoc escaped # markup in mustache template {{}} markup.
fixMustacheMarkup :: B.ByteString -> T.Text
fixMustacheMarkup content = fixMustacheMarkupText $ E.decodeUtf8 content

-- | Fixes pandoc escaped # markup in mustache template {{}} markup.
fixMustacheMarkupText :: T.Text -> T.Text
fixMustacheMarkupText content = T.replace (T.pack "{{\\#") (T.pack "{{#") content

-- | Substitutes meta data values in the provided file.
substituteMetaData
  :: FilePath -> MT.Value -> IO T.Text
substituteMetaData source metaData =
  do contents <- B.readFile source
     let fixed = fixMustacheMarkup contents
     let result =  M.compileTemplate source fixed
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
       makeRelative publicDir supportDir

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck
  :: FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out =
  do supportDir <- getRelativeSupportDir out
     let options =
           pandocWriterOpts {writerStandalone = True
               ,writerTemplate = deckTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
               -- ,writerHTMLMathMethod =
               --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
               --          (supportDir </> "katex-0.6.0/katex.min.css")
               ,writerVariables =
                  [("revealjs-url",supportDir </> "reveal.js")
                  ,("decker-support-dir",supportDir)]
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown markdownFile
     processed <- processPandocDeck "revealjs" pandoc
     writePandocString "revealjs" options out processed

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
-- template variables and calls need.
readAndPreprocessMarkdown :: FilePath -> Action Pandoc
readAndPreprocessMarkdown markdownFile =
  do projectDir <- getProjectDir
     let baseDir = takeDirectory markdownFile
     readMetaMarkdown markdownFile >>= processIncludes projectDir baseDir >>=
       populateCache

populateCache :: Pandoc -> Action Pandoc
populateCache pandoc =
  do cacheDir <- getCacheDir
     liftIO $ walkM (cacheRemoteImages cacheDir) pandoc

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage
  :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out =
  do supportDir <- getRelativeSupportDir out
     let options =
           pandocWriterOpts {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = pageTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
               -- ,writerHTMLMathMethod =
               --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
               --          (supportDir </> "katex-0.6.0/katex.min.css")
               ,writerVariables =
                  [("decker-support-dir",supportDir)]
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown markdownFile
     processed <- processPandocPage "html5" pandoc
     writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage
  :: FilePath -> FilePath -> Action ()
markdownToPdfPage markdownFile out =
  do let options =
           pandocWriterOpts {writerStandalone = True
               ,writerTemplate = pageLatexTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerCiteMethod = Citeproc}
     pandoc <- readAndPreprocessMarkdown markdownFile
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
  :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out =
  do pandoc <- readAndPreprocessMarkdown markdownFile
     processed <- processPandocHandout "html" pandoc
     supportDir <- getRelativeSupportDir out
     let options =
           pandocWriterOpts {writerHtml5 = True
               ,writerStandalone = True
               ,writerTemplate = handoutTemplate
               ,writerHighlight = True
               ,writerHighlightStyle = pygments
               ,writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
               -- ,writerHTMLMathMethod =
               --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
               --          (supportDir </> "katex-0.6.0/katex.min.css")
               ,writerVariables = [("decker-support-dir",supportDir)]
               ,writerCiteMethod = Citeproc}
     writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout
  :: FilePath -> FilePath -> Action ()
markdownToPdfHandout markdownFile out = do
    pandoc <- readAndPreprocessMarkdown markdownFile
    processed <- processPandocHandout "latex" pandoc
    let options =
            pandocWriterOpts
            { writerStandalone = True
            , writerTemplate = handoutLatexTemplate
            , writerHighlight = True
            , writerHighlightStyle = pygments
            , writerCiteMethod = Citeproc
            }
    putNormal $ "# pandoc (for " ++ out ++ ")"
    pandocMakePdf options processed out

readMetaMarkdown :: FilePath -> Action Pandoc
readMetaMarkdown markdownFile =
  do need [markdownFile]
     metaData <- readMetaDataForDir (takeDirectory markdownFile)
     pandoc <- liftIO $ readMetaMarkdownIO markdownFile metaData
     projectDir <- getProjectDir
     return $
       walk (adjustImageUrls projectDir
                             (takeDirectory markdownFile))
            pandoc

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism, if slides have duplicate titles in separate
-- include files.
deckerPandocExtensions :: Set.Set Extension
deckerPandocExtensions = Set.delete Ext_auto_identifiers pandocExtensions

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
    def
    { readerExtensions = deckerPandocExtensions
    }

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
    def
    { writerExtensions = deckerPandocExtensions
    }

readMetaMarkdownIO :: FilePath -> Y.Value -> IO Pandoc
readMetaMarkdownIO markdownFile metaData =
  do text <-
       substituteMetaData markdownFile
                          (MT.mFromJSON metaData)
     case readMarkdown pandocReaderOpts $ T.unpack text of
       Right pandoc -> return pandoc
       Left err -> throw $ PandocException (show err)

isLocalURI :: String -> Bool
isLocalURI url = isNothing $ parseURI url

isRemoteURI :: String -> Bool
isRemoteURI = not . isLocalURI

isCacheableURI :: String -> Bool
isCacheableURI url =
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:","https:"]
    Nothing -> False

-- | Walks over all images in a Pandoc document and transforms image URLs like
-- this: 1. Remote URLs are not transformed. 2. Absolute URLs are intepreted
-- relative to the project root directory. 3. Relative URLs are intepreted
-- relative to the containing document.
adjustImageUrls :: FilePath -> FilePath -> Pandoc -> Pandoc

adjustImageUrls projectDir baseDir =
    walk adjustBlock . walk adjustInline
  where
    adjustInline (Image attr inlines (url,title)) =
        Image attr inlines (adjustLocalUrl projectDir baseDir url, title)
    adjustInline other = other
    adjustBlock (Header 1 attr inlines) =
        Header 1 (adjustBgImageUrl attr) inlines
    adjustBlock other = other
    adjustBgImageUrl (i,cs,kvs) =
        ( i
        , cs
        , map
              (\(k,v) ->
                    if k == "data-background-image"
                        then (k, adjustLocalUrl projectDir baseDir v)
                        else (k, v))
              kvs)

adjustLocalUrl :: FilePath -> FilePath -> FilePath -> FilePath
adjustLocalUrl root base url
  | isLocalURI url =
      if isAbsolute url
          then root </> makeRelative "/" url
          else base </> url
adjustLocalUrl _ _ url = url

-- cacheRemoteImages :: FilePath -> [FilePath] -> [FilePath] -> Action ()
-- cacheRemoteImages cacheDir metaFiles markdownFiles =
--   do mapM_ cacheImages markdownFiles
--   where cacheImages markdownFile =
--           do pandoc <- readMetaMarkdown markdownFile
--              _ <- liftIO $ walkM (cachePandocImages cacheDir) pandoc
--              putNormal $ "# pandoc (cached images for " ++ markdownFile ++ ")"

-- Transitively splices all include files into the pandoc document.
processIncludes :: FilePath -> FilePath -> Pandoc -> Action Pandoc
processIncludes rootDir baseDir (Pandoc meta blocks) = do
    included <- processBlocks baseDir blocks
    return $ Pandoc meta included
  where
    processBlocks :: FilePath -> [Block] -> Action [Block]
    processBlocks base blcks = do
        spliced <- foldM (include base) [] blcks
        return $ concat $ reverse spliced
    include :: FilePath -> [[Block]] -> Block -> Action [[Block]]
    include base result (Para [Link _ [Str "#include"] (url,_)]) = do
        let filePath = adjustLocalUrl rootDir base url
        Pandoc _ b <- readMetaMarkdown filePath
        included <- processBlocks (takeDirectory filePath) b
        return $ included : result
    include _ result block = return $ [block] : result

cacheRemoteImages :: FilePath -> Pandoc -> IO Pandoc
cacheRemoteImages cacheDir = walkM cacheRemoteImage 
  where
    cacheRemoteImage (Image attr inlines (url,title)) = do
        cachedFile <- cacheRemoteFile cacheDir url
        return (Image attr inlines (cachedFile, title))
    cacheRemoteImage img = return img

cacheRemoteFile :: FilePath -> String -> IO FilePath
cacheRemoteFile cacheDir url
  | isCacheableURI url = do
      let cacheFile = cacheDir </> hashURI url
      exists <- Dir.doesFileExist cacheFile
      if exists
          then return cacheFile
          else catch
                   (do content <- downloadUrl url
                       createDirectoryIfMissing True cacheDir
                       LB.writeFile cacheFile content
                       return cacheFile)
                   (\e ->
                         do putStrLn $ "Warning: " ++ show (e :: SomeException)
                            return url)
cacheRemoteFile _ url = return url

clearCachedFile :: FilePath -> String -> IO ()
clearCachedFile cacheDir url
  | isCacheableURI url = do
      let cacheFile = cacheDir </> hashURI url
      exists <- Dir.doesFileExist cacheFile
      when exists $ removeFile cacheFile
clearCachedFile _ _ = return ()

downloadUrl :: String -> IO LB.ByteString
downloadUrl url = do
    request <- parseRequest url
    result <- httpLBS request
    let status = getResponseStatus result
    if status == ok200
        then return $ getResponseBody result
        else throw $
             HttpException $
             "Cannot download " ++
             url ++
             " (" ++
             show (statusCode status) ++
             " " ++ B.unpack (statusMessage status) ++ ")"

hashURI :: String -> String
hashURI uri = show (md5 $ L8.pack uri) SF.<.> SF.takeExtension uri

processPandocPage
  :: String -> Pandoc -> Action Pandoc
processPandocPage format pandoc = do
    let f = Just (Format format)
    -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages
    -- cacheDir)
    cacheDir <- getCacheDir
    processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
    return $ expandMacros f processed

processPandocDeck
  :: String -> Pandoc -> Action Pandoc
processPandocDeck format pandoc =
  do let f = Just (Format format)
     -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages
     -- cacheDir)
     cacheDir <- getCacheDir
     processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
     return $ (makeSlides f . expandMacros f) processed

processPandocHandout
  :: String -> Pandoc -> Action Pandoc
processPandocHandout format pandoc =
  do let f = Just (Format format)
     -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages
     -- cacheDir)
     cacheDir <- getCacheDir
     processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
     return $ (expandMacros f . filterNotes f) processed

type StringWriter = WriterOptions -> Pandoc -> String

writePandocString :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocString format options out pandoc =
  do let writer = getPandocWriter format
     final <- copyImages (takeDirectory out) pandoc
     writeFile' out
                (writer options final)
     putNormal $ "# pandoc for (" ++ out ++ ")"

copyImages :: FilePath -> Pandoc -> Action Pandoc
copyImages baseDir pandoc = do
    projectDir <- getProjectDir
    publicDir <- getPublicDir
    walkM (copyAndLinkInline projectDir publicDir) pandoc >>=
        walkM (copyAndLinkBlock projectDir publicDir)
  where
    copyAndLinkInline project public (Image attr inlines (url,title)) = do
        relUrl <- copyAndLinkFile project public baseDir url
        return (Image attr inlines (relUrl, title))
    copyAndLinkInline _ _ inline = return inline
    copyAndLinkBlock project public (Header 1 attr inlines) = do
        relAttr <- copyBgImageUrl project public attr
        return (Header 1 relAttr inlines)
    copyAndLinkBlock _ _ block = return block
    copyBgImageUrl project public (i,cs,kvs) = do
        relKvs <-
            mapM
                (\(k,v) ->
                      if k == "data-background-image"
                          then do
                              relUrl <-
                                  copyAndLinkFile project public baseDir v
                              return (k, relUrl)
                          else return (k, v))
                kvs
        return (i, cs, relKvs)

copyAndLinkFile :: FilePath -> FilePath -> FilePath -> FilePath -> Action FilePath
copyAndLinkFile project public base url = do
    let rel = makeRelative project url
    if rel == url
        then return url
        else do
            let pub = public </> rel
            liftIO $ createDirectoryIfMissing True (takeDirectory pub)
            copyFileChanged url pub
            return $ makeRelativeTo base pub

makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
    let (d,fd) =
            removeCommonPrefix (splitPath dir) (splitPath (takeDirectory file))
    in normalise $
       invertPath (joinPath d) </> joinPath fd </> takeFileName file
  where
    removeCommonPrefix al@(a:as) bl@(b:bs) =
        if a == b
            then removeCommonPrefix as bs
            else (al, bl)
    removeCommonPrefix a b = (a, b)

writeExampleProject :: Action ()
writeExampleProject = mapM_ writeOne deckerExampleDir
  where
    writeOne (path,contents) = do
        exists <- Development.Shake.doesFileExist path
        unless exists $
            do liftIO $ createDirectoryIfMissing True (takeDirectory path)
               liftIO $ B.writeFile path contents
               putNormal $ "# create (for " ++ path ++ ")"

writeEmbeddedFiles :: [(FilePath, B.ByteString)] -> FilePath -> Action ()
writeEmbeddedFiles files dir = do
    let absolute =
            map
                (\(path,contents) ->
                      (dir </> path, contents))
                files
    mapM_ write absolute
  where
    write (path,contents) = do
        liftIO $ Dir.createDirectoryIfMissing True (takeDirectory path)
        exists <- liftIO $ Dir.doesFileExist path
        unless exists $ liftIO $ B.writeFile path contents

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
  where
    lookup' :: [String] -> Maybe Y.Value -> Maybe String
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
    deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
    show (MustacheException e) = e
    show (HttpException e) = e
    show (PandocException e) = e
    show (YamlException e) = e
    show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
    show RsyncUrlException =
        "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
