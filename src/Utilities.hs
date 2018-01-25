{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Utilities
  ( runShakeInContext
  , watchFiles
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
  , writeEmbeddedFiles
  , getRelativeSupportDir
  , pandocMakePdf
  , fixMustacheMarkup
  , fixMustacheMarkupText
  , toPandocMeta
  , DeckerException(..)
  ) where

import Action
import Common
import Context
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.List as List
import Data.List.Extra as List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Yaml as Y
import Debug.Trace
import Development.Shake
import Development.Shake.FilePath as SFP
import Filter
import Macro
import Meta
import Network.URI
import Project
import Render
import Resources
import Server
import qualified System.Directory as Dir
import System.IO as S
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.PDF
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Watch

runShakeInContext :: ActionContext -> ShakeOptions -> Rules () -> IO ()
runShakeInContext context options rules = do
  opts <- setActionContext context options
  catch
    (untilM_ (tryRunShake opts) nothingToWatch)
    (\(SomeException e) -> putStrLn $ "Terminated: " ++ show e)
  cleanup
  where
    tryRunShake opts =
      handle (\(SomeException e) -> return ()) (shakeArgs opts rules)
    cleanup = do
      server <- readIORef $ ctxServerHandle context
      case server of
        Just handle -> stopHttpServer handle
        Nothing -> return ()
    nothingToWatch = do
      files <- readIORef $ ctxFilesToWatch context
      if null files
        then return True
        else do
          server <- readIORef $ ctxServerHandle context
          case server of
            Just handle -> reloadClients handle
            Nothing -> return ()
          _ <- waitForTwitchPassive files
          return False

watchFiles :: [FilePath] -> Action ()
watchFiles = setFilesToWatch

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

-- | Generates an index.md file with links to all generated files of interest.
writeIndex ::
     FilePath -> FilePath -> [FilePath] -> [FilePath] -> [FilePath] -> Action ()
writeIndex out baseUrl decks handouts pages = do
  let decksLinks = map (makeRelative baseUrl) decks
  let handoutsLinks = map (makeRelative baseUrl) handouts
  let pagesLinks = map (makeRelative baseUrl) pages
  dirs <- getProjectDirs
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ project dirs
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

substituteMetaData :: T.Text -> MT.Value -> T.Text
substituteMetaData text metaData = do
  let fixed = fixMustacheMarkupText text
  let result = M.compileTemplate "internal" fixed
  case result of
    Right template -> M.substituteValue template metaData
    Left err -> throw $ MustacheException (show err)

getTemplate :: FilePath -> Action String
getTemplate path = liftIO $ getResourceString ("template" </> path)

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from = do
  dirs <- getProjectDirs
  return $
    invertPath (makeRelative (public dirs) (takeDirectory from)) </>
    makeRelative (public dirs) (support dirs)

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out = do
  putCurrentDocument out
  supportDirRel <- getRelativeSupportDir out
  supportDir <- support <$> getProjectDirs
  template <- getTemplate "deck.html"
  need [supportDir </> "decker.css"]
  let options =
        pandocWriterOpts
        { writerSlideLevel = Just 1
        , writerTemplate = Just template
        -- , writerStandalone = True
        , writerHighlight = True
        -- , writerHighlightStyle = pygments
        , writerHTMLMathMethod =
            MathJax
              (supportDirRel </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        -- ,writerHTMLMathMethod =
        --    KaTeX (supportDirRel </> "katex-0.6.0/katex.min.js")
        --          (supportDirRel </> "katex-0.6.0/katex.min.css")
        , writerVariables =
            [ ("revealjs-url", supportDirRel </> "reveal.js-3.5.0")
            , ("decker-support-dir", supportDirRel)
            ]
        , writerCiteMethod = Citeproc
        }
  readAndProcessMarkdown markdownFile (Disposition Deck Html) >>=
    writePandocString "revealjs" options out

-- | Selects a matching pandoc string writer for the format string, or throws an
-- exception.
getPandocWriter :: String -> StringWriter
getPandocWriter format =
  case getWriter format of
    Right (PureStringWriter w) -> w
    Left e -> throw $ PandocException e
    _ -> throw $ PandocException $ "No writer for format: " ++ format

versionCheck :: Meta -> Action ()
versionCheck meta =
  unless isDevelopmentVersion $
  case lookupMeta "decker-version" meta of
    Just (MetaInlines version) -> check $ stringify version
    Just (MetaString version) -> check version
    _ ->
      putNormal $
      "  - Document version unspecified. This is decker version " ++
      deckerVersion ++ "."
  where
    check version =
      when (List.trim version /= List.trim deckerVersion) $
      putNormal $
      "  - Document version " ++
      version ++
      ". This is decker version " ++ deckerVersion ++ ". Expect problems."

-- | Reads a markdownfile, expands the included files, and substitutes mustache
-- template variables and calls need.
readAndProcessMarkdown :: FilePath -> Disposition -> Action Pandoc
readAndProcessMarkdown markdownFile disposition = do
  readMetaMarkdown markdownFile >>= processIncludes baseDir >>=
    processPandoc pipeline disposition
  where
    baseDir = takeDirectory markdownFile
    pipeline =
      concatM
        [ expandDeckerMacros
        , provisionResources baseDir
        , renderMediaTags
        , makeSlides
        , renderCodeBlocks
        , processCitesWithDefault
        , appendScripts
        ]
  -- Disable automatic caching of remote images for a while
  -- >>= walkM (cacheRemoteImages (cache dirs))

provisionResources :: FilePath -> Pandoc -> Decker Pandoc
provisionResources baseDir pandoc@(Pandoc meta _) =
  lift $ do
    let method = provisioningFromMeta meta
    mapMetaResources (provisionMetaResource method baseDir) pandoc >>=
      mapResources (provisionResource method baseDir)

lookupBool :: String -> Bool -> Meta -> Bool
lookupBool key def meta =
  case lookupMeta key meta of
    Just (MetaBool b) -> b
    _ -> def

provisionMetaResource ::
     Provisioning -> FilePath -> (String, FilePath) -> Action FilePath
provisionMetaResource method base (key, path)
  | key `elem` runtimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base path
    provisionResource method base filePath
provisionMetaResource method base (key, path)
  | key `elem` compiletimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base path
    need [filePath]
    return filePath
provisionMetaResource _ _ (key, path) = return path

-- | Determines if a URL can be resolved to a local file. Absolute file URLs are
-- resolved against and copied or linked to public from 
--    1. the project root 
--    2. the local filesystem root 
-- 
-- Relative file URLs are resolved against and copied or linked to public from 
--
--    1. the directory path of the referencing file 
--    2. the project root Copy and link operations target the public directory
--       in the project root and recreate the source directory structure. This
--       function is used to provision resources that are used at presentation
--       time.
--
-- Returns a public URL relative to base
provisionResource :: Provisioning -> FilePath -> FilePath -> Action FilePath
provisionResource provisioning base path =
  case parseRelativeReference path of
    Nothing -> return path
    Just uri -> do
      dirs <- getProjectDirs
      need [uriPath uri]
      let resource = resourcePathes dirs base uri
      publicResource <- getPublicResource
      withResource publicResource 1 $ do
        liftIO $
          case provisioning of
            Copy -> copyResource resource
            SymLink -> linkResource resource
            Absolute -> absRefResource resource
            Relative -> relRefResource base resource

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = do
  dirs <- getProjectDirs
  let rel = makeRelative (public dirs) out
  putNormal $ "# pandoc (for " ++ rel ++ ")"

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir out
  template <- getTemplate "page.html"
  let options =
        pandocWriterOpts
        { writerHtml5 = True
        , writerTemplate = Just template
        , writerHighlight = True
        , writerHTMLMathMethod =
            MathJax
              (supportDir </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        , writerVariables = [("decker-support-dir", supportDir)]
        , writerCiteMethod = Citeproc
        }
  readAndProcessMarkdown markdownFile (Disposition Page Html) >>=
    writePandocString "html5" options out

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage :: FilePath -> FilePath -> Action ()
markdownToPdfPage markdownFile out = do
  putCurrentDocument out
  template <- getTemplate "page.tex"
  let options =
        pandocWriterOpts
        { writerTemplate = Just template
        , writerHighlight = True
        , writerCiteMethod = Citeproc
        }
  readAndProcessMarkdown markdownFile (Disposition Page Pdf) >>=
    pandocMakePdf options out

pandocMakePdf :: WriterOptions -> FilePath -> Pandoc -> Action ()
pandocMakePdf options out pandoc = do
  result <- liftIO $ makePDF "pdflatex" writeLaTeX options pandoc
  case result of
    Left err -> throw $ PandocException (show err)
    Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir out
  template <- getTemplate "handout.html"
  let options =
        pandocWriterOpts
        { writerHtml5 = True
        , writerTemplate = Just template
        , writerHighlight = True
        , writerHTMLMathMethod =
            MathJax
              (supportDir </> "MathJax-2.7/MathJax.js?config=TeX-AMS_HTML")
        , writerVariables = [("decker-support-dir", supportDir)]
        , writerCiteMethod = Citeproc
        }
  readAndProcessMarkdown markdownFile (Disposition Handout Html) >>=
    writePandocString "html5" options out

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout :: FilePath -> FilePath -> Action ()
markdownToPdfHandout markdownFile out = do
  putCurrentDocument out
  template <- getTemplate "handout.tex"
  let options =
        pandocWriterOpts
        { writerTemplate = Just template
        , writerHighlight = True
        , writerCiteMethod = Citeproc
        }
  readAndProcessMarkdown markdownFile (Disposition Handout Pdf) >>=
    pandocMakePdf options out

-- | Reads a markdown file and returns a pandoc document. Handles meta data
-- extraction and template substitution. All references to local resources are
-- converted to absolute pathes.
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
  versionCheck (Meta m)
  let pandoc = Pandoc (Meta m) blocks
  -- adjust local media urls
  mapResources (urlToFilePathIfLocal (takeDirectory markdownFile)) pandoc

urlToFilePathIfLocal :: FilePath -> FilePath -> Action FilePath
urlToFilePathIfLocal base uri = do
  case parseRelativeReference uri of
    Nothing -> return uri
    Just relativeUri -> do
      let path = uriPath relativeUri
      absBase <- liftIO $ Dir.makeAbsolute base
      absRoot <- project <$> getProjectDirs
      let absPath =
            if isAbsolute path
              then absRoot </> makeRelative "/" path
              else absBase </> path
      return absPath

readMarkdownOrThrow :: ReaderOptions -> String -> Pandoc
readMarkdownOrThrow opts string =
  case readMarkdown opts string of
    Right pandoc -> pandoc
    Left err -> throw $ PandocException (show err)

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism if slides have duplicate titles in separate
-- include files.
deckerPandocExtensions :: Set.Set Extension
deckerPandocExtensions = Set.delete Ext_auto_identifiers pandocExtensions

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def {readerExtensions = deckerPandocExtensions}

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def {writerExtensions = deckerPandocExtensions}

mapResources :: (FilePath -> Action FilePath) -> Pandoc -> Action Pandoc
mapResources transform pandoc@(Pandoc meta blocks) = do
  processedBlocks <-
    walkM (mapInline transform) blocks >>= walkM (mapBlock transform)
  return (Pandoc meta processedBlocks)

mapAttributes :: (FilePath -> Action FilePath) -> Attr -> Action Attr
mapAttributes transform (ident, classes, kv) = do
  processed <- mapM mapAttr kv
  return (ident, classes, processed)
  where
    mapAttr kv@(key, value) =
      if key `elem` elementAttributes
        then do
          transformed <- transform value
          return (key, transformed)
        else return kv

mapInline :: (FilePath -> Action FilePath) -> Inline -> Action Inline
mapInline transform img@(Image attr@(_, cls, _) inlines (url, title)) = do
  a <- mapAttributes transform attr
  u <- transform url
  return $ Image a inlines (u, title)
mapInline transform lnk@(Link attr@(_, cls, _) inlines (url, title)) =
  if "resource" `elem` cls
    then do
      a <- mapAttributes transform attr
      u <- transform url
      return (Link a inlines (u, title))
    else return lnk
mapInline transform (Span attr inlines) = do
  attribs <- mapAttributes transform attr
  return (Span attribs inlines)
mapInline transform (Code attr string) = do
  attribs <- mapAttributes transform attr
  return (Code attribs string)
mapInline _ inline = return inline

mapBlock :: (FilePath -> Action FilePath) -> Block -> Action Block
mapBlock transform (CodeBlock attr string) = do
  attribs <- mapAttributes transform attr
  return (CodeBlock attribs string)
mapBlock transform (Header n attr inlines) = do
  attribs <- mapAttributes transform attr
  return (Header n attribs inlines)
mapBlock transform (Div attr blocks) = do
  attribs <- mapAttributes transform attr
  return (Div attribs blocks)
mapBlock _ block = return block

mapMetaResources ::
     ((String, FilePath) -> Action FilePath) -> Pandoc -> Action Pandoc
mapMetaResources transform (Pandoc (Meta kvmap) blocks) = do
  mapped <- mapM mapMeta $ Map.toList kvmap
  return $ Pandoc (Meta $ Map.fromList mapped) blocks
  where
    mapMeta (k, MetaString v)
      | k `elem` metaKeys = do
        transformed <- transform (k, v)
        return (k, MetaString transformed)
    mapMeta (k, MetaInlines inlines)
      | k `elem` metaKeys = do
        transformed <- transform (k, stringify inlines)
        return (k, MetaString transformed)
    mapMeta (k, MetaList l)
      | k `elem` metaKeys = do
        transformed <- mapM (mapMetaList k) l
        return (k, MetaList transformed)
    mapMeta kv = return kv
    mapMetaList k (MetaString v) = MetaString <$> transform (k, v)
    mapMetaList k (MetaInlines inlines) =
      MetaString <$> transform (k, stringify inlines)
    mapMetaList _ v = return v

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

-- | Resources in meta data that are needed at compile time. They have to be
-- specified as local URLs and must exist.
runtimeMetaKeys :: [String]
runtimeMetaKeys = ["css"]

compiletimeMetaKeys :: [String]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [String]
metaKeys = runtimeMetaKeys ++ compiletimeMetaKeys

-- Transitively splices all include files into the pandoc document.
processIncludes :: FilePath -> Pandoc -> Action Pandoc
processIncludes baseDir (Pandoc meta blocks) = do
  included <- processBlocks baseDir blocks
  return $ Pandoc meta included
  where
    processBlocks :: FilePath -> [Block] -> Action [Block]
    processBlocks base blcks = do
      spliced <- foldM (include base) [] blcks
      return $ concat $ reverse spliced
    include :: FilePath -> [[Block]] -> Block -> Action [[Block]]
    include base result (Para [Link _ [Str ":include"] (url, _)]) = do
      includeFile <- urlToFilePathIfLocal base url
      need [includeFile]
      Pandoc _ b <- readMetaMarkdown includeFile
      included <- processBlocks (takeDirectory includeFile) b
      return $ included : result
    include _ result block = return $ [block] : result

processCitesWithDefault :: Pandoc -> Decker Pandoc
processCitesWithDefault pandoc@(Pandoc meta blocks) =
  lift $ do
    document <-
      do case lookupMeta "csl" meta of
           Nothing -> do
             dir <- appData <$> getProjectDirs
             let defaultCsl = dir </> "template" </> "acm-sig-proceedings.csl"
             let cslMeta = setMeta "csl" (MetaString defaultCsl) meta
             return (Pandoc cslMeta blocks)
           _ -> return pandoc
    liftIO $ processCites' document

type StringWriter = WriterOptions -> Pandoc -> String

writePandocString :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocString format options out pandoc = do
  let writer = getPandocWriter format
  writeFile' out (writer options pandoc)

writeExampleProject :: Action ()
writeExampleProject = do
  liftIO $ writeResourceFiles "example" "."

{--
writeExampleProject :: Action ()
writeExampleProject = mapM_ writeOne deckerExampleDir
  where
    writeOne (path, contents) = do
      exists <- Development.Shake.doesFileExist path
      unless exists $ do
        liftIO $ Dir.createDirectoryIfMissing True (takeDirectory path)
        liftIO $ B.writeFile path contents
        putNormal $ "# create (for " ++ path ++ ")"
--}
writeEmbeddedFiles :: [(FilePath, B.ByteString)] -> FilePath -> Action ()
writeEmbeddedFiles files dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    putNormal $ "# write embedded files for (" ++ dir ++ ")"
    let absolute = map (first (dir </>)) files
    mapM_ write absolute
  where
    write (path, contents) = do
      liftIO $ Dir.createDirectoryIfMissing True (takeDirectory path)
      exists <- liftIO $ Dir.doesFileExist path
      unless exists $ liftIO $ B.writeFile path contents

lookupValue :: String -> Y.Value -> Maybe Y.Value
lookupValue key (Y.Object hashTable) = HashMap.lookup (T.pack key) hashTable
lookupValue _ _ = Nothing

metaValueAsString :: String -> Y.Value -> Maybe String
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
