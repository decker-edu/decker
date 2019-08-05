module Text.Decker.Reader.Markdown
  (readAndProcessMarkdown
  ) where

import Text.Decker.Filter.Filter
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Render
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Sketch
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Pandoc.Lens

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath as SFP
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Builder

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

-- | Fixes pandoc escaped # markup in mustache template {{}} markup.
fixMustacheMarkupText :: T.Text -> T.Text
fixMustacheMarkupText content =
  T.replace
    (T.pack "{{\\#")
    (T.pack "{{#")
    (T.replace (T.pack "{{\\^") (T.pack "{{^") content)

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
  let filePandoc@(Pandoc fileMeta _) =
        readMarkdownOrThrow pandocReaderOpts markdown
  let combinedMeta = mergePandocMeta fileMeta externalMeta
  let generateIds = lookupBool "generate-ids" False combinedMeta
  Pandoc fileMeta fileBlocks <- maybeGenerateIds generateIds filePandoc
  -- combine the meta data with preference on the embedded data
  let combinedMeta = mergePandocMeta fileMeta externalMeta
  let mustacheMeta = toMustacheMeta combinedMeta
   -- use mustache to substitute
  let substituted = substituteMetaData markdown mustacheMeta
  -- read markdown with substitutions again
  let Pandoc _ substitudedBlocks =
        readMarkdownOrThrow pandocReaderOpts substituted
  versionCheck combinedMeta
  let writeBack = lookupBool "write-back.enable" False combinedMeta
  when (generateIds || writeBack) $
    writeToMarkdownFile markdownFile (Pandoc fileMeta fileBlocks)
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

substituteMetaData :: T.Text -> MT.Value -> T.Text
substituteMetaData source metaData = do
  let fixed = fixMustacheMarkupText source
  let result = M.compileTemplate "internal" fixed
  case result of
    Right template -> M.substituteValue template metaData
    Left errMsg -> throw $ MustacheException (show errMsg)
