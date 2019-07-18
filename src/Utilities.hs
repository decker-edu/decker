{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Utilities
  ( runDecker
  , writeIndexLists
  , substituteMetaData
  , markdownToHtmlDeck
  , markdownToHtmlHandout
  , markdownToPdfHandout
  , markdownToHtmlPage
  , markdownToPdfPage
  , metaValueAsString
  , (<++>)
  , pandocMakePdf
  , fixMustacheMarkup
  , fixMustacheMarkupText
  , toPandocMeta
  , deckerPandocExtensions
  , readMarkdownOrThrow
  , pandocReaderOpts
  , DeckerException(..)
  ) where

import Common
import Exception
import Filter
import Macro
import Meta
import Output
import Project
import Quiz
import Render
import Resources
import Server
import Shake
import Sketch
import System.Decker.OS
import Text.Pandoc.Lens

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Lens ((^.), at)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.List as List
import Data.List.Extra as List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Development.Shake
import Development.Shake.FilePath as SFP
import Network.URI
import qualified System.Directory as Dir
import System.FilePath.Glob
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

-- | Generates an index.md file with links to all generated files of interest.
writeIndexLists :: FilePath -> FilePath -> Action ()
writeIndexLists out baseUrl = do
  dirs <- projectDirsA
  ts <- targetsA
  let decks = (zip (_decks ts) (_decksPdf ts))
  let handouts = (zip (_handouts ts) (_handoutsPdf ts))
  let pages = (zip (_pages ts) (_pagesPdf ts))
  decksLinks <- mapM makeLink decks
  handoutsLinks <- mapM makeLink handouts
  pagesLinks <- mapM makeLink pages
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ dirs ^. project
      , "---"
      , "# Slide decks"
      , unlines $ decksLinks
      , "# Handouts"
      , unlines $ handoutsLinks
      , "# Supporting Documents"
      , unlines $ pagesLinks
      ]
  where
    makeLink (html, pdf) = do
      pdfExists <- doesFileExist pdf
      if pdfExists
        then return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)
               (makeRelative baseUrl pdf)
        else return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)

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

-- TODO: Isn't imported anywhere
-- UNUSED:
-- only used once in Utilities.hs line 428
substituteMetaData :: T.Text -> MT.Value -> T.Text
substituteMetaData source metaData = do
  let fixed = fixMustacheMarkupText source
  let result = M.compileTemplate "internal" fixed
  case result of
    Right template -> M.substituteValue template metaData
    Left errMsg -> throw $ MustacheException (show errMsg)

getTemplate :: Meta -> Disposition -> Action String
getTemplate meta disp = do
  let templateOverridePath =
        case templateFromMeta meta of
          Just template -> Just $ template </> (getTemplateFileName disp)
          Nothing -> Nothing
  if isJust templateOverridePath
    then do
      let templateOverridePath' = fromJust templateOverridePath
      need [templateOverridePath']
      liftIO $ readFile templateOverridePath'
    else liftIO $ getResourceString ("template" </> (getTemplateFileName disp))

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc@(Pandoc meta body) =
  liftIO $
  runIOQuietly (writeNative pandocWriterOpts doc) >>= handleError >>=
  T.writeFile (out -<.> mod <.> ".hs")

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: FilePath -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out index = do
  let disp = Disposition Deck Html
  putCurrentDocument out
  supportDir <- _support <$> projectDirsA
  supportDirRel <- getRelativeSupportDir (takeDirectory out)
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  templateSupportDir <- getSupportDir meta out supportDirRel
  dachdeckerUrl' <- liftIO getDachdeckerUrl
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1
          , writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (supportDirRel </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables =
              [ ( "revealjs-url"
                , supportDirRel </> "node_modules" </> "reveal.js")
              , ("decker-support-dir", templateSupportDir)
              , ("dachdecker-url", dachdeckerUrl')
              ]
          , writerCiteMethod = Citeproc
          }
  let generateIds = fromMaybe False $ lookupMetaBool meta "generate-ids"
  when generateIds $ 
    writeDeckIndex markdownFile index pandoc
  -- writeNativeWhileDebugging out "filtered" pandoc
  writePandocFile "revealjs" options out pandoc

runIOQuietly :: PandocIO a -> IO (Either PandocError a)
runIOQuietly act = runIO (setVerbosity ERROR >> act)

writePandocFile :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile fmt options out pandoc =
  liftIO $
  case getWriter fmt of
    Right (TextWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      B.writeFile out . E.encodeUtf8
    Right (ByteStringWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      LB.writeFile out
    Left e -> throw $ PandocException e

-- TODO: Move to Common? since much of the version checking is done there (Meta is from Pandoc)
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
readAndProcessMarkdown markdownFile disp = do
  pandoc@(Pandoc meta _) <-
    readMetaMarkdown markdownFile >>= processIncludes baseDir
  processed@(Pandoc meta body) <-
    processPandoc pipeline baseDir disp (provisioningFromMeta meta) pandoc
  return processed
  where
    baseDir = takeDirectory markdownFile
    pipeline =
      concatM
        [ expandDeckerMacros
        , renderCodeBlocks
        , includeCode
        , provisionResources
        , renderQuizzes
        , processSlides
        , renderMediaTags
        , extractFigures
        , processCitesWithDefault
        , appendScripts
        ]
  -- REVIEW: Disable automatic caching of remote images for a while
  -- >>= walkM (cacheRemoteImages (cache dirs))

-- | Determines which template file name to use
-- for a certain disposition type
getTemplateFileName :: Disposition -> String
getTemplateFileName (Disposition Deck Html) = "deck.html"
getTemplateFileName (Disposition Deck Latex) = "deck.tex"
getTemplateFileName (Disposition Page Html) = "page.html"
getTemplateFileName (Disposition Page Latex) = "page.tex"
getTemplateFileName (Disposition Handout Html) = "handout.html"
getTemplateFileName (Disposition Handout Latex) = "handout.tex"

-- TODO: provisionResources could stay here since it uses Pandoc/Decker Pandoc
-- This probably does not need to be introduced to Resources module
provisionResources :: Pandoc -> Decker Pandoc
provisionResources pandoc = do
  base <- gets basePath
  method <- gets provisioning
  lift $
    mapMetaResources (provisionMetaResource base method) pandoc >>=
    mapResources (provisionResource base method)

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = do
  public <- publicA
  let rel = makeRelative public out
  putNormal $ "# pandoc (for " ++ rel ++ ")"

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Page Html
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  templateSupportDir <- getSupportDir meta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          }
  writePandocFile "html5" options out pandoc

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage :: FilePath -> FilePath -> Action ()
markdownToPdfPage markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Page Latex
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc

pandocMakePdf :: WriterOptions -> FilePath -> Pandoc -> Action ()
pandocMakePdf options out pandoc =
  liftIO $ do
    result <-
      runIOQuietly (makePDF "xelatex" [] writeLaTeX options pandoc) >>=
      handleError
    case result of
      Left errMsg -> throw $ PandocException (show errMsg)
      Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Handout Html
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  templateSupportDir <- getSupportDir meta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          }
  writePandocFile "html5" options out pandoc

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout :: FilePath -> FilePath -> Action ()
markdownToPdfHandout markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Handout Latex
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc

-- | Reads a markdown file and returns a pandoc document. Handles meta data
-- extraction and template substitution. All references to local resources are
-- converted to absolute pathes.
readMetaMarkdown :: FilePath -> Action Pandoc
readMetaMarkdown markdownFile = do
  projectDir <- projectA
  need [markdownFile]
  -- read external meta data for this directory
  externalMeta <-
    liftIO $
    toPandocMeta <$> aggregateMetaData projectDir (takeDirectory markdownFile)
  markdown <- liftIO $ T.readFile markdownFile
  let filePandoc@(Pandoc fileMeta fileBlocks) = readMarkdownOrThrow pandocReaderOpts markdown
  let combinedMeta = mergePandocMeta fileMeta externalMeta
  let generateIds = fromMaybe False $ lookupMetaBool combinedMeta "generate-ids"
  Pandoc fileMeta fileBlocks <- maybeGenerateIds generateIds filePandoc
  -- combine the meta data with preference on the embedded data
  let mustacheMeta = toMustacheMeta combinedMeta
   -- use mustache to substitute
  let substituted = substituteMetaData markdown mustacheMeta
  -- read markdown with substitutions again
  let Pandoc _ substitudedBlocks =
        readMarkdownOrThrow pandocReaderOpts substituted
  versionCheck combinedMeta
  let writeBack =
        fromMaybe False $ lookupMetaBool combinedMeta "write-back.enable"
  when (generateIds || writeBack) $
    liftIO $ writeToMarkdownFile markdownFile (Pandoc fileMeta fileBlocks)
  mapResources
    (urlToFilePathIfLocal (takeDirectory markdownFile))
    (Pandoc combinedMeta substitudedBlocks)
  where
    maybeGenerateIds doit pandoc =
      if doit
        then liftIO $ provideSlideIds pandoc
        else return pandoc

readMarkdownOrThrow :: ReaderOptions -> T.Text -> Pandoc
readMarkdownOrThrow opts markdown =
  case runPure (readMarkdown opts markdown) of
    Right pandoc -> pandoc
    Left errMsg -> throw $ PandocException (show errMsg)

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism if slides have duplicate titles in separate
-- include files.
deckerPandocExtensions :: Extensions
deckerPandocExtensions =
  (disableExtension Ext_auto_identifiers .
   disableExtension Ext_simple_tables . disableExtension Ext_multiline_tables)
    pandocExtensions

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def {readerExtensions = deckerPandocExtensions}

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def {writerExtensions = deckerPandocExtensions}

mapResources :: (FilePath -> Action FilePath) -> Pandoc -> Action Pandoc
mapResources transform (Pandoc meta blocks) =
  Pandoc meta <$> walkM (mapInline transform) blocks >>=
  walkM (mapBlock transform)

mapAttributes :: (FilePath -> Action FilePath) -> Attr -> Action Attr
mapAttributes transform (ident, classes, kvs) = do
  processed <- mapM mapAttr kvs
  return (ident, classes, processed)
  where
    mapAttr kv@(key, value) =
      if key `elem` elementAttributes
        then do
          transformed <- transform value
          return (key, transformed)
        else return kv

mapInline :: (FilePath -> Action FilePath) -> Inline -> Action Inline
mapInline transform (Image attr inlines (url, title)) = do
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

-- TODO: Move to Meta.hs?
-- UNUSED:
-- only used once her: line 313
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

-- Transitively splices all include files into the pandoc document.
processIncludes :: FilePath -> Pandoc -> Action Pandoc
processIncludes baseDir (Pandoc meta blocks) =
  Pandoc meta <$> processBlocks baseDir blocks
  where
    processBlocks :: FilePath -> [Block] -> Action [Block]
    processBlocks base blcks =
      concat . reverse <$> foldM (include base) [] blcks
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
      case lookupMeta "csl" meta of
        Nothing -> do
          dir <- appDataA
          let defaultCsl = dir </> "template" </> "acm-sig-proceedings.csl"
          let cslMeta = setMeta "csl" (MetaString defaultCsl) meta
          return (Pandoc cslMeta blocks)
        _ -> return pandoc
    liftIO $ processCites' document

lookupValue :: String -> Y.Value -> Maybe Y.Value
lookupValue key (Y.Object hashTable) = HashMap.lookup (T.pack key) hashTable
lookupValue _ _ = Nothing

-- TODO: move to Meta.hs?
-- used in Decker.hs
metaValueAsString :: String -> Y.Value -> Maybe String
metaValueAsString key meta =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (lookupValue k meta)
  where
    lookup' :: [String] -> Maybe Y.Value -> Maybe String
    lookup' [] (Just (Y.String s)) = Just (T.unpack s)
    lookup' [] (Just (Y.Number n)) = Just (show n)
    lookup' [] (Just (Y.Bool b)) = Just (show b)
    lookup' (k:ks) (Just obj@(Y.Object _)) = lookup' ks (lookupValue k obj)
    lookup' _ _ = Nothing
