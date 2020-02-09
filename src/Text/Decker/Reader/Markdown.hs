module Text.Decker.Reader.Markdown (readAndProcessMarkdown) where

import Text.Decker.Filter.Filter
import Text.Decker.Filter.IncludeCode
import Text.Decker.Filter.Macro
import Text.Decker.Filter.MarioMedia
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Render
import Text.Decker.Filter.ShortLink
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Pandoc.Lens
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath as SFP
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import System.Directory

-- Transitively splices all include files into the pandoc document.
processIncludes :: FilePath -> Pandoc -> Action Pandoc
processIncludes baseDir (Pandoc meta blocks) =
    Pandoc meta <$> processBlocks baseDir blocks
  where
    processBlocks :: FilePath -> [Block] -> Action [Block]
    processBlocks base blcks =
        Prelude.concat . Prelude.reverse <$> foldM (include base) [] blcks

    include :: FilePath -> [[Block]] -> Block -> Action [[Block]]
    include base result (Para [Link _ [Str ":include"] (url,_)]) = do
        includeFile <- urlToFilePathIfLocal base (T.unpack url )
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
    pandoc @ (Pandoc meta _)
        <- readMetaMarkdown markdownFile >>= processIncludes baseDir
    processPandoc
        (pipeline meta)
        baseDir
        disp
        (provisioningFromMeta meta)
        pandoc
  where
    baseDir = takeDirectory markdownFile

    pipeline meta = case getMetaBool "mario" meta of
        Just True -> concatM
            [ evaluateShortLinks
            , expandDeckerMacros
            , renderCodeBlocks
            , includeCode
            , provisionResources
            , renderQuizzes
            , processSlides
            , marioMedia
            , processCitesWithDefault
            , appendScripts]
        _ -> concatM
            [ evaluateShortLinks
            , expandDeckerMacros
            , renderCodeBlocks
            , includeCode
            , provisionResources
            , renderQuizzes
            , processSlides
            , renderMediaTags
            , extractFigures
            , processCitesWithDefault
            , appendScripts]

-- | Reads a markdown file and returns a pandoc document. Handles meta data
-- extraction and template substitution. All references to local resources are
-- converted to absolute pathes.
readMetaMarkdown :: FilePath -> Action Pandoc
readMetaMarkdown markdownFile = do
    projectDir <- projectA
    need [markdownFile]
    markdown <- liftIO $ T.readFile markdownFile
    -- Global meta data for this directory from decker.yaml and specified additional files
    globalMeta <- globalMetaA
    let filePandoc @ (Pandoc fileMeta fileBlocks) =
          readMarkdownOrThrow pandocReaderOpts markdown
    additionalMeta <- getAdditionalMeta fileMeta
    let combinedMeta = mergePandocMeta' additionalMeta globalMeta
    versionCheck combinedMeta
    let writeBack = getMetaBoolOrElse "write-back.enable" False combinedMeta
    when (writeBack)
        $ writeToMarkdownFile markdownFile (Pandoc fileMeta fileBlocks)
    mapResources
        (urlToFilePathIfLocal (takeDirectory markdownFile))
        (Pandoc combinedMeta fileBlocks)

readMarkdownOrThrow :: ReaderOptions -> T.Text -> Pandoc
readMarkdownOrThrow opts markdown =
    case runPure (readMarkdown opts markdown) of
        Right pandoc -> pandoc
        Left errMsg -> throw $ PandocException (show errMsg)

-- | Writes a pandoc document atoimically to a markdown file. 
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc@(Pandoc pmeta _) = do
  template <- getTemplate' "template/deck.md"
  let columns = getMetaIntOrElse "write-back.line-columns" 80 pmeta
  let wrapOpt "none" = WrapNone
      wrapOpt "preserve" = WrapPreserve
      wrapOpt _ = WrapAuto
  let wrap = getMetaTextOrElse "write-back.line-wrap" "none" pmeta
  let extensions =
        (disableExtension Ext_simple_tables .
         disableExtension Ext_multiline_tables .
         disableExtension Ext_grid_tables .
         disableExtension Ext_raw_html . enableExtension Ext_auto_identifiers)
          pandocExtensions
  let options =
        def
          { writerTemplate = Just template
          , writerExtensions = extensions
          , writerColumns = columns
          , writerWrapText = wrapOpt wrap
          , writerSetextHeaders = False
          }
  markdown <- liftIO $ runIO (writeMarkdown options pandoc) >>= handleError
  fileContent <- liftIO $ T.readFile filepath
  when (markdown /= fileContent) $
    withTempFile
      (\tmp -> liftIO $ T.writeFile tmp markdown >> renameFile tmp filepath)

processCitesWithDefault :: Pandoc -> Decker Pandoc
processCitesWithDefault = lift . liftIO . processCites'

substituteMetaData :: T.Text -> MT.Value -> T.Text
substituteMetaData source metaData = do
    let fixed = fixMustacheMarkupText source
    let result = M.compileTemplate "internal" fixed
    case result of
        Right template -> M.substituteValue template metaData
        Left errMsg -> throw $ MustacheException (show errMsg)
