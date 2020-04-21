module Text.Decker.Reader.Markdown
  ( readAndProcessMarkdown
  ) where

import Text.Decker.Filter.Decker
import Text.Decker.Filter.Filter
import Text.Decker.Filter.IncludeCode
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Render
import Text.Decker.Filter.ShortLink
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake hiding (Resource)
import Development.Shake.FilePath as SFP
import System.Directory
import Text.CSL.Pandoc
import Text.Pandoc

-- Transitively splices all include files into the pandoc document.
processIncludes :: Meta -> FilePath -> Pandoc -> Action Pandoc
processIncludes globalMeta topBaseDir (Pandoc meta blocks) =
  Pandoc meta <$> processBlocks blocks
  where
    processBlocks :: [Block] -> Action [Block]
    processBlocks blcks =
      Prelude.concat . Prelude.reverse <$> foldM include [] blcks
    include :: [[Block]] -> Block -> Action [[Block]]
    include result (Para [Link _ [Str ":include"] (url, _)]) = do
      includeFile <- urlToFilePathIfLocal topBaseDir (T.unpack url)
      need [includeFile]
      Pandoc _ b <- readMetaMarkdown globalMeta topBaseDir includeFile
      included <- processBlocks b
      return $ included : result
    include result block = return $ [block] : result

-- | Runs a new style decker filter. That means
--
-- 1. Put the decker path info into the documents meta data
-- 2. Run the filter.
-- 3. Take the resource info from the document meta data and
--    a) Call need on every dependency.
--    b) Provision the resources (copy to public)
-- 4. Remove all traces of this from the meta data
-- 
runDeckerFilter ::
     (Pandoc -> IO Pandoc) -> FilePath -> FilePath -> Pandoc -> Action Pandoc
runDeckerFilter filter topBase docBase pandoc@(Pandoc meta blocks) = do
  -- | Augment document meta.
  let deckerMeta =
        setTextMetaValue "decker.base-dir" (T.pack docBase) $
        setTextMetaValue "decker.top-base-dir" (T.pack topBase) meta
  (Pandoc resultMeta resultBlocks) <- liftIO $ filter (Pandoc deckerMeta blocks)
  processedMeta <- processMeta resultMeta
  return (Pandoc processedMeta resultBlocks)
  where
    processMeta meta = do
      let (resources :: [(Text, Resource)]) =
            fromMaybe [] $
            getMetaValue "decker.filter.resources" meta >>= fromMetaValue
      mapM_ (processResource . snd) resources
      -- TODO Remove resource info from meta data
      return meta
      where
        processResource resource@(Resource source target url) = do
          let method = provisioningFromMeta meta
          need [source]
          publishResource method topBase resource

-- | Runs the new decker media filter.
deckerMediaFilter topBase docBase (Pandoc meta blocks) =
  runDeckerFilter (mediaFilter options) topBase docBase (Pandoc meta blocks)
  where
    options =
      def
        { writerTemplate = Nothing
        , writerHTMLMathMethod = MathJax "Handled by reveal.js in the template"
        , writerExtensions =
            (enableExtension Ext_auto_identifiers . enableExtension Ext_emoji)
              pandocExtensions
        }

-- | The old style decker filter pipeline with Mario's media handling.
marioPipeline =
  concatM
    [ evaluateShortLinks
    , expandDeckerMacros
    , renderCodeBlocks
    , includeCode
    , provisionResources
    , renderQuizzes
    , processSlides
    , processCitesWithDefault
    ]

-- | The old style decker filter pipeline.
deckerPipeline =
  concatM
    [ evaluateShortLinks
    , expandDeckerMacros
    , renderCodeBlocks
    , includeCode
    , provisionResources
    , renderQuizzes
    , processSlides
    , processCitesWithDefault
    ]

-- | Reads a markdownfile, expands the included files, and calls need.
readAndProcessMarkdown :: Meta -> FilePath -> Disposition -> Action Pandoc
readAndProcessMarkdown meta markdownFile disp
  -- TODO decide whether absolute or relative is better
  -- let topLevelBase = makeRelative projectDir $ takeDirectory markdownFile
 = do
  topLevelBase <- liftIO $ makeAbsolute $ takeDirectory markdownFile
  let provisioning = provisioningFromMeta meta
  readMetaMarkdown meta topLevelBase markdownFile >>=
    processIncludes meta topLevelBase >>=
    processPandoc deckerPipeline topLevelBase disp provisioning

-- | Reads a markdown file and returns a pandoc document. Handles meta data
-- extraction and template substitution. All references to local resources are
-- converted to absolute pathes.
readMetaMarkdown :: Meta -> FilePath -> FilePath -> Action Pandoc
readMetaMarkdown globalMeta topLevelBase markdownFile = do
  projectDir <- projectA
  docBase <- liftIO $ makeAbsolute $ takeDirectory markdownFile
  need [markdownFile]
  markdown <- liftIO $ T.readFile markdownFile
  let filePandoc@(Pandoc fileMeta fileBlocks) =
        readMarkdownOrThrow pandocReaderOpts markdown
  additionalMeta <- getAdditionalMeta docBase fileMeta
  let combinedMeta = mergePandocMeta' additionalMeta globalMeta
  versionCheck combinedMeta
  let writeBack = getMetaBoolOrElse "write-back.enable" False combinedMeta
  when writeBack $ writeToMarkdownFile markdownFile (Pandoc fileMeta fileBlocks)
  -- This is the new media filter. Runs right after reading. Because every matched
  -- document fragment is converted to raw HTML, the following old style filters
  -- will not see them.
  filtered <-
    deckerMediaFilter topLevelBase docBase (Pandoc combinedMeta fileBlocks)
  -- TODO remove once old style filters are migrated
  mapResources (urlToFilePathIfLocal topLevelBase) filtered

readMarkdownOrThrow :: ReaderOptions -> T.Text -> Pandoc
readMarkdownOrThrow opts markdown =
  case runPure (readMarkdown opts markdown) of
    Right pandoc -> pandoc
    Left errMsg -> throw $ PandocException (show errMsg)

-- | Writes a pandoc document atomically to a markdown file. 
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc@(Pandoc pmeta _) = do
  template <-
    liftIO
      (compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n\n$body$" >>=
       handleLeftM)
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
