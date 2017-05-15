module Utilities
  ( calcProjectDirectory
  , spawn
  , terminate
  , threadDelay'
  , wantRepeat
  , defaultContext
  , runShakeInContext
  , watchFiles
  , dropSuffix
  , stopServer
  , startServer
  , runHttpServer
  , writeIndex
  , readMetaDataForDir
  , substituteMetaData
  , markdownToHtmlDeck
  , markdownToHtmlHandout
  , markdownToPdfHandout
  , markdownToHtmlPage
  , markdownToPdfPage
  , writeExampleProject
  , metaValueAsString
  , (<++>)
  , markNeeded
  , replaceSuffixWith
  , writeEmbeddedFiles
  , getRelativeSupportDir
  , pandocMakePdf
  , isCacheableURI
  , adjustLocalUrl
  , cacheRemoteFile
  , cacheRemoteImages
  , makeRelativeTo
  , fixMustacheMarkup
  , fixMustacheMarkupText
  , globA
  , globRelA
  , splitMarkdown
  , splitMarkdownFile
  , toPandocMeta
  , DeckerException(..)
  ) where

import Context
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Digest.Pure.MD5
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashMap.Strict as H
import Data.IORef
import Data.List
import Data.List.Extra
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Scientific
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as Tio
import Data.Time.Clock
import Data.Typeable
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Debug.Trace
import Development.Shake
import Development.Shake.FilePath as SFP
import Embed
import Filter
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI
import System.Directory as Dir
import System.Exit
import System.FilePath as SF
import System.FilePath.Glob
import System.IO as S
import System.Process
import System.Process.Internals
import Text.CSL.Pandoc
import Text.Highlighting.Kate.Styles
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.PDF
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Watch

-- Find the project directory and change current directory to there. 
-- The project directory is the first upwards directory that contains a .git directory entry.
calcProjectDirectory :: IO FilePath
calcProjectDirectory = do
  cwd <- getCurrentDirectory
  searchGitRoot cwd
  where
    searchGitRoot :: FilePath -> IO FilePath
    searchGitRoot path =
      if isDrive path
        then makeAbsolute "."
        else do
          hasGit <- Dir.doesDirectoryExist (path </> ".git")
          if hasGit
            then makeAbsolute path
            else searchGitRoot $ takeDirectory path

-- | Globs for files under the project dir in the Action monad. 
-- Returns absolute pathes. 
globA :: FilePattern -> Action [FilePath]
globA pat = do
  projectDir <- getProjectDir
  liftIO $ globDir1 (compile pat) projectDir

-- | Globs for files under the project dir in the Action monad. 
-- Returns pathes relative to the project directory. 
globRelA :: FilePattern -> Action [FilePath]
globRelA pat = do
  projectDir <- getProjectDir
  files <- globA pat
  return $ map (makeRelative projectDir) files

-- Utility functions for shake based apps
spawn :: String -> Action ProcessHandle
spawn = liftIO . spawnCommand

-- Runs liveroladx on the given directory, if it is not already running. If
-- open is True a browser window is opended.
runHttpServer dir open = do
  process <- getServerHandle
  case process of
    Just handle -> return ()
    Nothing -> do
      putNormal "# livereloadx (on http://localhost:8888, see server.log)"
      handle <-
        spawn $ "livereloadx -s -p 8888 -d 500 " ++ dir ++ " 2>&1 > server.log"
      setServerHandle $ Just handle
      threadDelay' 200000
      when open $ cmd ("open http://localhost:8888/" :: String) :: Action ()

startServer id command =
  liftIO $ do
    processHandle <- spawnCommand command
    withProcessHandle processHandle handleResult
  where
    handleResult ph =
      case ph of
        ClosedHandle e ->
          print $ "Error starting server " ++ id ++ ": " ++ show e
        OpenHandle p -> do
          print $ "Server " ++ id ++ " running (" ++ show p ++ ")"
          writeFile (id ++ ".pid") (show p)

stopServer id =
  liftIO $ do
    let pidFile = id ++ ".pid"
    result <- try $ readFile pidFile
    case result of
      Left (SomeException e) -> print $ "Unable to read file " ++ pidFile
      Right pid -> do
        exitCode <- system ("kill -9 " ++ pid)
        removeFile pidFile

terminate :: ProcessHandle -> Action ()
terminate = liftIO . terminateProcess

threadDelay' :: Int -> Action ()
threadDelay' = liftIO . threadDelay

wantRepeat :: IORef Bool -> Action ()
wantRepeat justOnce = liftIO $ writeIORef justOnce False

-- The context of program invocation consists of a list of
-- files to watch and a possibly running local http server.
data Context =
  Context [FilePath]
          (Maybe ProcessHandle)

defaultContext = Context [] Nothing

runShakeInContext :: ActionContext -> ShakeOptions -> Rules () -> IO ()
runShakeInContext context options rules = do
  opts <- setActionContext context options
  catch
    (untilM_ (tryRunShake opts) nothingToWatch)
    (\(SomeException e) -> putStrLn $ "Terminated: " ++ show e)
  cleanup
  where
    tryRunShake opts =
      catch (shakeArgs opts rules) (\(SomeException e) -> return ())
    cleanup = do
      process <- readIORef $ ctxServerHandle context
      case process of
        Just handle -> terminateProcess handle
        Nothing -> return ()
    nothingToWatch = do
      files <- readIORef $ ctxFilesToWatch context
      if null files
        then return True
        else do
          waitForTwitchPassive files
          return False

watchFiles = setFilesToWatch

-- | Monadic version of list concatenation.
(<++>)
  :: Monad m
  => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

-- | Mark files as need and return them
markNeeded :: [FilePath] -> Action [FilePath]
markNeeded files = do
  need files
  return files

-- | Removes the last suffix from a filename
dropSuffix s t = fromMaybe t (stripSuffix s t)

-- | Monadic version of suffix replacement for easy binding.
replaceSuffixWith :: String -> String -> [FilePath] -> Action [FilePath]
replaceSuffixWith suffix with pathes =
  return [dropSuffix suffix d ++ with | d <- pathes]

-- | Monadic version of suffix replacement for easy binding.
calcTargetPath :: FilePath
               -> String
               -> String
               -> [FilePath]
               -> Action [FilePath]
calcTargetPath projectDir suffix with pathes =
  return [projectDir </> dropSuffix suffix d ++ with | d <- pathes]

-- | Generates an index.md file with links to all generated files of interest.
writeIndex out baseUrl decks handouts pages = do
  let decksLinks = map (makeRelative baseUrl) decks
  let handoutsLinks = map (makeRelative baseUrl) handouts
  let pagesLinks = map (makeRelative baseUrl) pages
  projectDir <- getProjectDir
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ projectDir
      , "---"
      , "# Slide decks"
      , unlines $ map makeLink $ sort decksLinks
      , "# Handouts"
      , unlines $ map makeLink $ sort handoutsLinks
      , "# Supporting Documents"
      , unlines $ map makeLink $ sort pagesLinks
      ]
  where
    makeLink path = "-    [" ++ takeFileName path ++ "](" ++ path ++ ")"

joinMeta :: Y.Value -> Y.Value -> Y.Value
joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
joinMeta (Y.Object old) _ = Y.Object old
joinMeta _ (Y.Object new) = Y.Object new
joinMeta _ _ = throw $ YamlException "Can only join YAML objects."

readMetaDataForDir :: FilePath -> Action Y.Value
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
    decodeYaml yamlFile = do
      result <- liftIO $ Y.decodeFileEither yamlFile
      case result of
        Right object@(Y.Object _) -> return object
        Right _ ->
          throw $
          YamlException $ "Top-level meta value must be an object: " ++ dir
        Left exception -> throw exception

-- | Fixes pandoc escaped # markup in mustache template {{}} markup.
fixMustacheMarkup :: B.ByteString -> T.Text
fixMustacheMarkup content = fixMustacheMarkupText $ E.decodeUtf8 content

-- | Fixes pandoc escaped # markup in mustache template {{}} markup.
fixMustacheMarkupText :: T.Text -> T.Text
fixMustacheMarkupText content =
  T.replace
    (T.pack "{{\\#")
    (T.pack "{{#")
    (T.replace (T.pack "{{\\^") (T.pack "{{^") content)

-- | Substitutes meta data values in the provided file.
substituteMetaDataFile :: FilePath -> MT.Value -> IO T.Text
substituteMetaDataFile source metaData = do
  contents <- B.readFile source
  let fixed = fixMustacheMarkup contents
  let result = M.compileTemplate source fixed
  case result of
    Right template -> return $ M.substituteValue template metaData
    Left err -> throw $ MustacheException (show err)

substituteMetaData :: T.Text -> MT.Value -> T.Text
substituteMetaData text metaData = do
  let fixed = fixMustacheMarkupText text
  let result = M.compileTemplate "internal" fixed
  case result of
    Right template -> M.substituteValue template metaData
    Left err -> throw $ MustacheException (show err)

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from = do
  supportDir <- getSupportDir
  publicDir <- getPublicDir
  return $
    invertPath (makeRelative publicDir (takeDirectory from)) </>
    makeRelative publicDir supportDir

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out = do
  supportDir <- getRelativeSupportDir out
  let options =
        pandocWriterOpts
        { writerTemplate = Just deckTemplate
        -- , writerStandalone = True
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerHTMLMathMethod =
            MathJax
              (supportDir </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        -- ,writerHTMLMathMethod =
        --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
        --          (supportDir </> "katex-0.6.0/katex.min.css")
        , writerVariables =
            [ ("revealjs-url", supportDir </> "reveal.js")
            , ("decker-support-dir", supportDir)
            ]
        , writerCiteMethod = Citeproc
        }
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
readAndPreprocessMarkdown markdownFile = do
  projectDir <- getProjectDir
  let baseDir = takeDirectory markdownFile
  readMetaMarkdown markdownFile >>= processIncludes projectDir baseDir >>=
    populateCache

populateCache :: Pandoc -> Action Pandoc
populateCache pandoc = do
  cacheDir <- getCacheDir
  liftIO $ walkM (cacheRemoteImages cacheDir) pandoc

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out = do
  supportDir <- getRelativeSupportDir out
  let options =
        pandocWriterOpts
        { writerHtml5 = True
        -- , writerStandalone = True
        , writerTemplate = Just pageTemplate
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerHTMLMathMethod =
            MathJax
              (supportDir </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        -- ,writerHTMLMathMethod =
        --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
        --          (supportDir </> "katex-0.6.0/katex.min.css")
        , writerVariables = [("decker-support-dir", supportDir)]
        , writerCiteMethod = Citeproc
        }
  pandoc <- readAndPreprocessMarkdown markdownFile
  processed <- processPandocPage "html5" pandoc
  writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage :: FilePath -> FilePath -> Action ()
markdownToPdfPage markdownFile out = do
  let options =
        pandocWriterOpts
        { writerTemplate = Just pageLatexTemplate
        -- , writerStandalone = True
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerCiteMethod = Citeproc
        }
  pandoc <- readAndPreprocessMarkdown markdownFile
  processed <- processPandocPage "latex" pandoc
  putNormal $ "# pandoc (for " ++ out ++ ")"
  pandocMakePdf options processed out

pandocMakePdf options processed out = do
  result <- liftIO $ makePDF "pdflatex" writeLaTeX options processed
  case result of
    Left err -> throw $ PandocException (show err)
    Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out = do
  pandoc <- readAndPreprocessMarkdown markdownFile
  processed <- processPandocHandout "html" pandoc
  supportDir <- getRelativeSupportDir out
  let options =
        pandocWriterOpts
        { writerHtml5 = True
        -- , writerStandalone = True
        , writerTemplate = Just handoutTemplate
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerHTMLMathMethod =
            MathJax
              (supportDir </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        -- ,writerHTMLMathMethod =
        --    KaTeX (supportDir </> "katex-0.6.0/katex.min.js")
        --          (supportDir </> "katex-0.6.0/katex.min.css")
        , writerVariables = [("decker-support-dir", supportDir)]
        , writerCiteMethod = Citeproc
        }
  writePandocString "html5" options out processed

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout :: FilePath -> FilePath -> Action ()
markdownToPdfHandout markdownFile out = do
  pandoc <- readAndPreprocessMarkdown markdownFile
  processed <- processPandocHandout "latex" pandoc
  let options =
        pandocWriterOpts
        { writerTemplate = Just handoutLatexTemplate
        -- , writerStandalone = True
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerCiteMethod = Citeproc
        }
  putNormal $ "# pandoc (for " ++ out ++ ")"
  pandocMakePdf options processed out

splitMarkdownFile :: FilePath -> Action (T.Text, Y.Value)
splitMarkdownFile file = do
  markdown <- liftIO $ Tio.readFile file
  return $ splitMarkdown markdown

splitMarkdown :: T.Text -> (T.Text, Y.Value)
splitMarkdown markdown =
  partition $
  map (T.strip . T.unlines) $ split (T.pack "---" ==) $ T.lines markdown
  where
    partition = foldl tryYaml (T.pack "", Y.Object H.empty)
    tryYaml (text, meta) block =
      case Y.decodeEither (E.encodeUtf8 block) of
        Right yaml@(Y.Object _) -> (text, joinMeta yaml meta)
        _ -> ((T.strip . T.unlines) [text, T.pack "", block], meta)

-- | Reads a markdown file and returns a pandoc document. 
readMetaMarkdown :: FilePath -> Action Pandoc
readMetaMarkdown markdownFile = do
  need [markdownFile]
  -- read external meta data for this directory
  externalMeta <- readMetaDataForDir (takeDirectory markdownFile)
  -- extract embedded meta data from the document
  markdown <- liftIO $ S.readFile markdownFile
  let Pandoc meta _ = readMarkdownOrThrow pandocReaderOpts markdown
  let documentMeta = MetaMap $ unMeta meta
  -- combine the meta data with preference on the embedded data
  let combinedMeta = mergePandocMeta documentMeta (toPandocMeta externalMeta)
  let mustacheMeta = toMustacheMeta combinedMeta
   -- use mustache to substitute
  let substituted = substituteMetaData (T.pack markdown) mustacheMeta
  -- read markdown with substitutions again
  let Pandoc _ blocks =
        readMarkdownOrThrow pandocReaderOpts $ T.unpack substituted
  let (MetaMap m) = combinedMeta
  let pandoc = Pandoc (Meta m) blocks
  -- adjust image urls
  projectDir <- getProjectDir
  return $ walk (adjustImageUrls projectDir (takeDirectory markdownFile)) pandoc
  where
    readMarkdownOrThrow opts string =
      case readMarkdown opts string of
        Right pandoc -> pandoc
        Left err -> throw $ PandocException (show err)

-- | Converts YAML meta data to Pandoc meta data
toPandocMetaMeta :: Y.Value -> Meta
toPandocMetaMeta value =
  case toPandocMeta value of
    MetaMap meta -> Meta meta
    _ ->
      throw $
      YamlException "toPandocMeta: expected MetaMap, got some other MetaValue"

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are rendered to 
-- markdown strings with default options.
toMustacheMeta :: MetaValue -> MT.Value
toMustacheMeta (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta) $ Map.toList mmap
toMustacheMeta (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta a
toMustacheMeta (MetaBool bool) = MT.Bool bool
toMustacheMeta (MetaString string) = MT.String $ T.pack string
toMustacheMeta (MetaInlines inlines) =
  MT.String $
  T.pack $ writeMarkdown def (Pandoc (Meta Map.empty) [(Plain inlines)])
toMustacheMeta (MetaBlocks blocks) =
  MT.String $ T.pack $ writeMarkdown def (Pandoc (Meta Map.empty) blocks)

mergePandocMeta :: MetaValue -> MetaValue -> MetaValue
mergePandocMeta (MetaMap left) (MetaMap right) = MetaMap $ Map.union left right
mergePandocMeta left _ = left

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> MetaValue
toPandocMeta (Y.Object m) =
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta) $ H.toList m
toPandocMeta (Y.Array vector) = MetaList $ map toPandocMeta $ Vec.toList vector
toPandocMeta (Y.String text) = MetaString $ T.unpack text
toPandocMeta (Y.Number scientific) = MetaString $ show scientific
toPandocMeta (Y.Bool bool) = MetaBool bool
toPandocMeta (Y.Null) = MetaList []

toUtf8String :: T.Text -> String
toUtf8String = B.unpack . E.encodeUtf8

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism, if slides have duplicate titles in separate
-- include files.
deckerPandocExtensions :: Set.Set Extension
deckerPandocExtensions = Set.delete Ext_auto_identifiers pandocExtensions

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def {readerExtensions = deckerPandocExtensions}

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def {writerExtensions = deckerPandocExtensions}

readMetaMarkdownIO :: FilePath -> Y.Value -> IO Pandoc
readMetaMarkdownIO markdownFile metaData = do
  text <- substituteMetaDataFile markdownFile (MT.mFromJSON metaData)
  case readMarkdown pandocReaderOpts $ toUtf8String text of
    Right pandoc -> return pandoc
    Left err -> throw $ PandocException (show err)

isLocalURI :: String -> Bool
isLocalURI url = isNothing $ parseURI url

isRemoteURI :: String -> Bool
isRemoteURI = not . isLocalURI

isCacheableURI :: String -> Bool
isCacheableURI url =
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:", "https:"]
    Nothing -> False

-- | Walks over all images in a Pandoc document and transforms image URLs like
-- this: 1. Remote URLs are not transformed. 2. Absolute URLs are intepreted
-- relative to the project root directory. 3. Relative URLs are intepreted
-- relative to the containing document.
adjustImageUrls :: FilePath -> FilePath -> Pandoc -> Pandoc
adjustImageUrls projectDir baseDir = walk adjustBlock . walk adjustInline
  where
    adjustInline (Image attr inlines (url, title)) =
      Image attr inlines (adjustLocalUrl projectDir baseDir url, title)
    adjustInline other = other
    adjustBlock (Header 1 attr inlines) =
      Header 1 (adjustBgImageUrl attr) inlines
    adjustBlock other = other
    adjustBgImageUrl (i, cs, kvs) =
      ( i
      , cs
      , map
          (\(k, v) ->
             if k == "data-background-image" || k == "data-background-video"
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
    include base result (Para [Link _ [Str "#include"] (url, _)]) = do
      let filePath = adjustLocalUrl rootDir base url
      Pandoc _ b <- readMetaMarkdown filePath
      included <- processBlocks (takeDirectory filePath) b
      return $ included : result
    include _ result block = return $ [block] : result

cacheRemoteImages :: FilePath -> Pandoc -> IO Pandoc
cacheRemoteImages cacheDir = walkM cacheRemoteImage
  where
    cacheRemoteImage (Image attr inlines (url, title)) = do
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
             (\e -> do
                putStrLn $ "Warning: " ++ show (e :: SomeException)
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

processPandocPage :: String -> Pandoc -> Action Pandoc
processPandocPage format pandoc = do
  let f = Just (Format format)
  cacheDir <- getCacheDir
  -- processed <-
  --   liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
  processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
  return $ expandMacros f processed

processPandocDeck :: String -> Pandoc -> Action Pandoc
processPandocDeck format pandoc = do
  let f = Just (Format format)
  cacheDir <- getCacheDir
  -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
  processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
  return $ (makeSlides f . expandMacros f) processed

processPandocHandout :: String -> Pandoc -> Action Pandoc
processPandocHandout format pandoc = do
  let f = Just (Format format)
  cacheDir <- getCacheDir
  -- processed <- liftIO $ processCites' pandoc >>= walkM (useCachedImages cacheDir)
  processed <- liftIO $ walkM (useCachedImages cacheDir) pandoc
  return $ (expandMacros f . filterNotes f) processed

type StringWriter = WriterOptions -> Pandoc -> String

writePandocString :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocString format options out pandoc = do
  let writer = getPandocWriter format
  final <- copyImages (takeDirectory out) pandoc
  writeFile' out (writer options final)
  putNormal $ "# pandoc for (" ++ out ++ ")"

copyImages :: FilePath -> Pandoc -> Action Pandoc
copyImages baseDir pandoc = do
  projectDir <- getProjectDir
  publicDir <- getPublicDir
  walkM (copyAndLinkInline projectDir publicDir) pandoc >>=
    walkM (copyAndLinkBlock projectDir publicDir)
  where
    copyAndLinkInline project public (Image attr inlines (url, title)) = do
      relUrl <- copyAndLinkFile project public baseDir url
      return (Image attr inlines (relUrl, title))
    copyAndLinkInline _ _ inline = return inline
    copyAndLinkBlock project public (Header 1 attr inlines) = do
      relAttr <- copyBgImageUrl project public attr
      return (Header 1 relAttr inlines)
    copyAndLinkBlock _ _ block = return block
    copyBgImageUrl project public (i, cs, kvs) = do
      relKvs <-
        mapM
          (\(k, v) ->
             if k == "data-background-image"
               then do
                 relUrl <- copyAndLinkFile project public baseDir v
                 return (k, relUrl)
               else return (k, v))
          kvs
      return (i, cs, relKvs)

copyAndLinkFile :: FilePath
                -> FilePath
                -> FilePath
                -> FilePath
                -> Action FilePath
copyAndLinkFile project public base url = do
  let rel = makeRelative project url
  if rel == url
    then return url
    else do
      let pub = public </> rel
      liftIO $ createDirectoryIfMissing True (takeDirectory pub)
      copyFileChanged url pub
      return $ makeRelativeTo base pub

-- | Express the second path argument as relative to the first. 
-- Both arguments are expected to be absolute pathes. 
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
  let (d, f) = removeCommonPrefix (splitDirectories dir) (splitDirectories file)
  in normalise $ invertPath (joinPath d) </> joinPath f

removeCommonPrefix :: [FilePath] -> [FilePath] -> ([FilePath], [FilePath])
removeCommonPrefix al@(a:as) bl@(b:bs)
  | a == b = removeCommonPrefix as bs
  | otherwise = (al, bl)
removeCommonPrefix [] b = ([], b)

writeExampleProject :: Action ()
writeExampleProject = mapM_ writeOne deckerExampleDir
  where
    writeOne (path, contents) = do
      exists <- Development.Shake.doesFileExist path
      unless exists $ do
        liftIO $ createDirectoryIfMissing True (takeDirectory path)
        liftIO $ B.writeFile path contents
        putNormal $ "# create (for " ++ path ++ ")"

writeEmbeddedFiles :: [(FilePath, B.ByteString)] -> FilePath -> Action ()
writeEmbeddedFiles files dir = do
  let absolute = map (\(path, contents) -> (dir </> path, contents)) files
  mapM_ write absolute
  where
    write (path, contents) = do
      liftIO $ Dir.createDirectoryIfMissing True (takeDirectory path)
      exists <- liftIO $ Dir.doesFileExist path
      unless exists $ liftIO $ B.writeFile path contents

lookupValue :: String -> Y.Value -> Maybe Y.Value
lookupValue key (Y.Object hashTable) = HashMap.lookup (T.pack key) hashTable
lookupValue key _ = Nothing

metaValueAsString :: String -> Y.Value -> Maybe String
metaValueAsString key meta =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (lookupValue k meta)
  where
    lookup' :: [String] -> Maybe Y.Value -> Maybe String
    lookup' [] (Just (Y.String text)) = Just (toUtf8String text)
    lookup' [] (Just (Y.Number n)) = Just (show n)
    lookup' [] (Just (Y.Bool b)) = Just (show b)
    lookup' (k:ks) (Just obj@(Y.Object _)) = lookup' ks (lookupValue k obj)
    lookup' _ _ = Nothing

-- | Tool specific exceptions
data DeckerException
  = MustacheException String
  | GitException String
  | PandocException String
  | YamlException String
  | HttpException String
  | RsyncUrlException
  | DecktapeException String
  deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (GitException e) = e
  show (HttpException e) = e
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
