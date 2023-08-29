{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Reader.Markdown
  ( readAndFilterMarkdownFile,
    readMarkdownFile,
    readDeckerMeta,
    readMetaData,
    processCites,
    deckerMediaFilter,
    expandMeta,
    mergeDocumentMeta,
  )
where

import Control.Monad
import Control.Monad.Loops
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Maybe
import Data.Text.IO qualified as Text
import Development.Shake hiding (Resource)
import Relude
import System.Directory qualified as Dir
import System.FilePath.Posix
import Text.Decker.Exam.Filter
import Text.Decker.Filter.Decker2
import Text.Decker.Filter.Detail
import Text.Decker.Filter.Filter
import Text.Decker.Filter.IncludeCode
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Paths
import Text.Decker.Filter.Poll
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.ShortLink
import Text.Decker.Filter.Template (expandTemplateMacros)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template
import Text.Decker.Writer.CSS (computeCssColorVariables, computeCssVariables)
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Citeproc
import Text.Pandoc.Shared

-- | Reads a Markdown file and run all the the Decker specific filters on it.
-- The path is assumed to be an absolute path in the local file system under
-- the project root directory. Throws an exception if something goes wrong
readAndFilterMarkdownFile :: Disposition -> Meta -> FilePath -> Action Pandoc
readAndFilterMarkdownFile disp globalMeta path = do
  let docBase = takeDirectory path
  readMarkdownFile globalMeta path
    >>= mergeDocumentMeta globalMeta
    >>= processMeta
    >>= processCites
    >>= calcRelativeResourcePaths docBase
    >>= runDynamicFilters Before docBase
    >>= runNewFilter disp examinerFilter docBase
    >>= deckerMediaFilter disp docBase
    >>= processPandoc (deckerPipeline disp) docBase disp
    >>= runDynamicFilters After docBase

processMeta (Pandoc meta blocks) = do
  let processed = computeCssColorVariables $ computeCssVariables meta
  return (Pandoc processed blocks)

-- |  TODO: Provide default CSL data from the resources if csl: is not set. This
--  is not really trivial.
processCites :: MonadIO m => Pandoc -> m Pandoc
processCites pandoc@(Pandoc meta blocks) = liftIO $ do
  if
      | isMetaSet "bibliography" meta && isMetaSet "csl" meta ->
          runIOorExplode $ processCitations pandoc
      | isMetaSet "bibliography" meta -> do
          defaultCSL <- installDefaultCSL
          let cslMeta = setMetaValue "csl" defaultCSL meta
          runIOorExplode $ processCitations (Pandoc cslMeta blocks)
      | otherwise -> return pandoc

-- |  TODO: This seems to fail sometimes with j > 1. Some race condition. Maybe
--  call need and write a rule for the extraction.
installDefaultCSL :: IO FilePath
installDefaultCSL = do
  let path = transientDir </> "default.csl"
  exists <- Dir.doesFileExist path
  unless exists $ do
    csl <- readResource "default.csl" (DeckerExecutable "decker")
    BS.writeFile path (fromJust csl)
  return path

-- | Reads a Markdown file from the local file system. Local resource paths are
-- converted to absolute paths. Additional meta data is read and merged into
-- the document. Other Markdown files may be transitively included. Throws an
-- exception if something goes wrong
readMarkdownFile :: Meta -> FilePath -> Action Pandoc
readMarkdownFile globalMeta path = do
  let base = takeDirectory path
  parseMarkdownFile path
    >>= writeBack globalMeta path
    >>= expandMeta globalMeta base
    >>= adjustResourcePathsA base
    >>= checkVersion
    >>= includeMarkdownFiles globalMeta base
    >>= addPathInfo base

addPathInfo :: FilePath -> Pandoc -> Action Pandoc
addPathInfo documentPath (Pandoc meta blocks) = do
  let pathToProject = makeRelativeTo documentPath "."
  let pathToSupport = makeRelativeTo documentPath "support"
  let meta' =
        addMetaField "projectPath" pathToProject $
          addMetaField "supportPath" pathToSupport meta
  return (Pandoc meta' blocks)

-- | Parses a Markdown file and throws an exception if something goes wrong.
parseMarkdownFile :: FilePath -> Action Pandoc
parseMarkdownFile path = do
  markdown <- liftIO $ Text.readFile path
  liftIO $ runIOorExplode (readMarkdown pandocReaderOpts markdown)

-- | Writes a Pandoc document to a file in Markdown format. Throws an exception
-- if something goes wrong
writeBack :: Meta -> FilePath -> Pandoc -> Action Pandoc
writeBack meta path pandoc = do
  let writeBack = lookupMetaOrElse False "write-back.enable" meta
  when writeBack $ writeToMarkdownFile path pandoc
  return pandoc

-- | Reads additional meta data from files listed in `meta-data:`. In case of
-- conflict, order of encounter determines preference. Later values win.
expandMeta :: Meta -> FilePath -> Pandoc -> Action Pandoc
expandMeta globalMeta base (Pandoc docMeta content) = do
  expanded <-
    adjustMetaPaths globalMeta base docMeta
      >>= readAdditionalMeta globalMeta base
  return (Pandoc expanded content)

-- | Adjusts meta data values that reference local files needed at run-time (by
-- some plugin, presumeably) and at compile-time (by some template). Lists of
-- these variables can be specified in the meta data.
adjustMetaPaths :: Meta -> FilePath -> Meta -> Action Meta
adjustMetaPaths globalMeta base meta = do
  let variables = pathVariables globalMeta
  adjustMetaVariables (adjust base) variables meta
  where
    adjust base path = do
      let apath = makeProjectPath base (toString path)
      -- putStrLn $ "adjustMetaPaths: base: " <> base <> ", path: " <> toString path <> ", adjusted: " <> apath
      return $ toText apath

-- | Adjusts meta data values that reference files needed at run-time (by some
-- plugin, presumeably) and at compile-time (by some template). Lists of these
-- variables can be specified in the meta data.
needMetaTargets :: FilePath -> Meta -> Action Meta
needMetaTargets base meta =
  adjustMetaVariables (adjustR base) (runtimePathVariables meta) meta
    >>= adjustMetaVariables (adjustC base) (compiletimePathVariables meta)
  where
    adjustR base path = do
      let stringPath = toString path
      need [publicDir </> stringPath]
      let relativePath = makeRelativeTo base stringPath
      -- putStrLn $ "needMetaTargets: base: " <> base <> ", path: " <> toString path <> ", adjusted: " <> relativePath
      return $ toText relativePath
    adjustC base path = do
      let pathString = toString path
      isDir <- liftIO $ Dir.doesDirectoryExist pathString
      unless isDir (need [pathString])
      return path

adjustMetaVariables :: (Text -> Action Text) -> [Text] -> Meta -> Action Meta
adjustMetaVariables action keys meta = foldM func meta keys
  where
    func meta' key = adjustMetaStringsBelowM action key meta'

-- | Merges global meta data into the document. Document meta values have
-- preference.
mergeDocumentMeta :: Meta -> Pandoc -> Action Pandoc
mergeDocumentMeta globalMeta (Pandoc docMeta content) = do
  let combinedMeta = mergePandocMeta docMeta globalMeta
  return (Pandoc combinedMeta content)

checkVersion :: Pandoc -> Action Pandoc
checkVersion = return

-- | Traverses the pandoc AST and transitively embeds included Markdown files.
includeMarkdownFiles :: Meta -> FilePath -> Pandoc -> Action Pandoc
includeMarkdownFiles globalMeta docBase (Pandoc docMeta content) =
  Pandoc docMeta <$> processBlocks content
  where
    processBlocks :: [Block] -> Action [Block]
    processBlocks blcks = concat . reverse <$> foldM include [] blcks
    include :: [[Block]] -> Block -> Action [[Block]]
    include document (Para [Link _ [Str ":include"] (url, _)]) = do
      let path = makeProjectPath docBase (toString url)
      -- putVerbose $ "# --> include: " <> toString url <> " (" <> path <> ")"
      need [path]
      Pandoc _ includedBlocks <- readMarkdownFile globalMeta path
      return $ includedBlocks : document
    include document block = return $ [block] : document

pathVariables :: Meta -> [Text]
pathVariables meta =
  List.nub $ compiletimePathVariables meta <> runtimePathVariables meta

-- TODO: what does this even mean?
compiletimePathVariables :: Meta -> [Text]
compiletimePathVariables meta =
  List.nub $
    [ "csl",
      "bibliography",
      "meta-data",
      "static-resource-dirs",
      "static-resources",
      "extra-highlight-syntax"
    ]
      <> lookupMetaOrElse [] "compiletime-path-variables" meta

-- TODO: what does this even mean?
runtimePathVariables :: Meta -> [Text]
runtimePathVariables meta =
  List.nub $ ["template"] <> lookupMetaOrElse [] "runtime-path-variables" meta

-- | Calculates paths relative to docBase for all runtime paths contained in
-- the meta data. Also calls need on those files.
calcRelativeResourcePaths :: FilePath -> Pandoc -> Action Pandoc
calcRelativeResourcePaths base (Pandoc meta content) = do
  calculated <- needMetaTargets base meta
  return (Pandoc calculated content)

-- | Check for additional meta files specified in the Meta option `meta-data`.
-- Assumes that files are specified with absolute paths.
readAdditionalMeta :: Meta -> FilePath -> Meta -> Action Meta
readAdditionalMeta globalMeta base meta = do
  let metaFiles = lookupMetaOrElse [] "meta-data" meta :: [String]
  putVerbose $ "# --> readAdditionalMeta: " <> show metaFiles
  moreMeta <- traverse (readMetaData globalMeta) metaFiles
  return $ foldr mergePandocMeta meta (reverse moreMeta)

-- | Reads a meta data file. All values that are paths to local project files
-- are made absolute. Files referenced in `meta-data` are recursively loaded
-- and merged.
readMetaData :: Meta -> FilePath -> Action Meta
readMetaData globalMeta path = do
  need [path]
  putVerbose $ "# --> readMetaData: " <> path
  let base = takeDirectory path
  meta <- liftIO $ readMetaDataFile path
  adjustMetaPaths globalMeta base meta >>= readAdditionalMeta globalMeta base

readDeckerMeta :: FilePath -> Action Meta
readDeckerMeta file = do
  deckerMeta <- readMetaData nullMeta file
  argsMeta <- readMetaData nullMeta metaArgsFile
  let baseMeta = mergePandocMeta argsMeta deckerMeta
  defaultMeta <- readTemplateMeta baseMeta
  return $ mergePandocMeta baseMeta defaultMeta

-- | Runs a new style decker filter. That means
--
-- 1. Put the decker path info into the documents meta data
-- 2. Run the filter.
-- 3. Take the resource info from the document meta data and
--    a) Call need on every dependency.
--    b) Provision the resources (copy to public)
-- 4. Remove all traces of this from the meta data
runDeckerFilter :: (Pandoc -> IO Pandoc) -> FilePath -> Pandoc -> Action Pandoc
runDeckerFilter filter docBase pandoc@(Pandoc docMeta blocks) = do
  let deckerMeta = setMetaValue "decker.base-dir" docBase docMeta
  (Pandoc resultMeta resultBlocks) <- liftIO $ filter (Pandoc deckerMeta blocks)
  need (lookupMetaOrElse [] "decker.filter.resources" resultMeta)
  return (Pandoc docMeta resultBlocks)

runNewFilter :: Disposition -> (Pandoc -> Filter Pandoc) -> FilePath -> Pandoc -> Action Pandoc
runNewFilter dispo filter docBase pandoc@(Pandoc docMeta blocks) = do
  let deckerMeta = setMetaValue "decker.base-dir" docBase docMeta
  (Pandoc resultMeta resultBlocks) <-
    liftIO $ runFilter2 dispo filter (Pandoc deckerMeta blocks)
  need (lookupMetaOrElse [] "decker.filter.resources" resultMeta)
  return (Pandoc docMeta resultBlocks)

-- |  Runs the new decker media filter.
deckerMediaFilter :: Disposition -> String -> Pandoc -> Action Pandoc
deckerMediaFilter dispo docBase pandoc@(Pandoc meta _) =
  runDeckerFilter (mediaFilter2 dispo) docBase pandoc

-- |  The old style decker filter pipeline.
deckerPipeline (Disposition Deck Html) =
  concatM
    [ evaluateShortLinks,
      expandTemplateMacros,
      expandDeckerMacros,
      includeCode,
      processDetailDiv,
      processSlides,
      handlePolls,
      handleQuizzes
    ]
deckerPipeline (Disposition Page Html) =
  concatM
    [ evaluateShortLinks,
      expandTemplateMacros,
      expandDeckerMacros,
      includeCode,
      processDetailDiv,
      processDetailHeader
    ]
deckerPipeline (Disposition Index Html) =
  concatM
    [ evaluateShortLinks,
      expandTemplateMacros,
      expandDeckerMacros,
      includeCode,
      processDetailDiv,
      processDetailHeader
    ]
deckerPipeline (Disposition Handout Html) =
  concatM
    [ evaluateShortLinks,
      expandTemplateMacros,
      expandDeckerMacros,
      includeCode,
      processDetailDiv,
      processSlides
    ]
deckerPipeline disp = error $ "Disposition not supported: " <> show disp

-- | Writes a pandoc document atomically to a markdown file.
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc@(Pandoc pmeta _) = do
  template <-
    liftIO
      ( compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n\n$body$"
          >>= handleLeftM
      )
  let columns = lookupMetaOrElse 80 "write-back.line-columns" pmeta
  let wrapOpt :: Text -> WrapOption
      wrapOpt "none" = WrapNone
      wrapOpt "preserve" = WrapPreserve
      wrapOpt _ = WrapAuto
  let wrap = lookupMetaOrElse "none" "write-back.line-wrap" pmeta
  let extensions =
        ( disableExtension Ext_simple_tables
            . disableExtension Ext_multiline_tables
            . disableExtension Ext_grid_tables
            . disableExtension Ext_raw_html
            . enableExtension Ext_auto_identifiers
        )
          pandocExtensions
  let options =
        def
          { writerTemplate = Just template,
            writerExtensions = extensions,
            writerColumns = columns,
            writerWrapText = wrapOpt wrap,
            writerSetextHeaders = False
          }
  markdown <- liftIO $ runIO (writeMarkdown options pandoc) >>= handleError
  fileContent <- liftIO $ Text.readFile filepath
  when (markdown /= fileContent) $
    withTempFile
      (\tmp -> liftIO $ Text.writeFile tmp markdown >> Dir.renameFile tmp filepath)
