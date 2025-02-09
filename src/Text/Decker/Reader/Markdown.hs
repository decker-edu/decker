{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Reader.Markdown
  ( readAndFilterMarkdownFile,
    readMarkdownFile,
    processCites,
    formatStdin,
  )
where

import Control.Monad
import Control.Monad.Loops
import Data.Maybe
import Data.Text.IO qualified as Text
import Development.Shake hiding (Resource)
import Relude
import System.AtomicWrite.Writer.ByteString
import System.Directory qualified as Dir
import System.FilePath.Posix
import Text.Decker.Exam.Filter
import Text.Decker.Filter.Detail
import Text.Decker.Filter.Filter
import Text.Decker.Filter.FragmentTemplate (expandFragmentTemplates)
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Paths
import Text.Decker.Filter.Poll
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Select (filterSelectedSlides)
import Text.Decker.Filter.ShortLink
import Text.Decker.Filter.Decker2
import Text.Decker.Filter.Template (expandTemplateMacros)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Decker.Resource.Resource
import Text.Decker.Writer.CSS (computeCssColorVariables, computeCssVariables)
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Citeproc
import Text.Pandoc.Shared
import Text.Decker.Internal.MetaExtra (mergeDocumentMeta, expandMeta, needMetaTargets)

-- | Reads a Markdown file and run all the the Decker specific filters on it.
-- The path is assumed to be an absolute path in the local file system under
-- the project root directory. Throws an exception if something goes wrong
readAndFilterMarkdownFile :: Disposition -> Meta -> FilePath -> Action Pandoc
readAndFilterMarkdownFile disp globalMeta docPath = do
  let docBase = takeDirectory docPath
  readMarkdownFile globalMeta docPath
    >>= mergeDocumentMeta globalMeta
    >>= processMeta
    >>= processCites
    >>= runNewFilter disp filterSelectedSlides docPath
    >>= calcRelativeResourcePaths docBase
    >>= runDynamicFilters Before docBase
    >>= runNewFilter disp examinerFilter docPath
    >>= runNewFilter disp expandTemplateMacros docPath
    >>= runNewFilter disp expandFragmentTemplates docPath
    >>= deckerMediaFilter disp docPath
    >>= processPandoc (deckerPipeline disp) docBase disp
    >>= runDynamicFilters After docBase

processMeta (Pandoc meta blocks) = do
  let processed = computeCssColorVariables $ computeCssVariables meta
  return (Pandoc processed blocks)

-- | Provide default CSL data from the resources if csl: is not set.
processCites :: (MonadIO m) => Pandoc -> m Pandoc
processCites pandoc@(Pandoc meta blocks) = liftIO $ do
  if
    | isMetaSet "bibliography" meta && isMetaSet "csl" meta ->
        runIOorExplode $ processCitations pandoc
    | isMetaSet "bibliography" meta -> do
        defaultCSL <- installDefaultCSL
        let cslMeta = setMetaValue "csl" defaultCSL meta
        runIOorExplode $ processCitations (Pandoc cslMeta blocks)
    | otherwise -> return pandoc

installDefaultCSL :: IO FilePath
installDefaultCSL = do
  transient <- transientDir
  let path = transient </> "default.csl"
  exists <- Dir.doesFileExist path
  unless exists $ do
    csl <- readResource "default.csl" (DeckerExecutable "decker")
    atomicWriteFile path (fromJust csl)
  return path

-- | Reads a Markdown file from the local file system. Local resource paths are
-- converted to absolute paths. Additional meta data is read and merged into
-- the document. Other Markdown files may be transitively included. Throws an
-- exception if something goes wrong
readMarkdownFile :: Meta -> FilePath -> Action Pandoc
readMarkdownFile globalMeta path = do
  let base = takeDirectory path
  parseMarkdownFile path
    -- >>= (\(Pandoc meta blocks) ->
    --         do  putStrLn $ path <> "\n" <> show meta
    --             return (Pandoc meta blocks))
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
        addMetaField "projectPath" pathToProject
          $ addMetaField "supportPath" pathToSupport
          $ addMetaField "documentPath" documentPath meta
  return (Pandoc meta' blocks)

-- | Parses a Markdown file and throws an exception if something goes wrong.
parseMarkdownFile :: FilePath -> Action Pandoc
parseMarkdownFile path = do
  markdown <- liftIO $ Text.readFile path
  liftIO $ parseMarkdown markdown

parseMarkdown :: Text -> IO Pandoc
parseMarkdown text = runIOorExplode (readMarkdown pandocReaderOpts text)

-- | Writes a Pandoc document to a file in Markdown format. Throws an exception
-- if something goes wrong
writeBack :: Meta -> FilePath -> Pandoc -> Action Pandoc
writeBack meta path pandoc@(Pandoc docMeta _) = do
  let writeBack :: Bool = lookupMetaOrElse (lookupMetaOrElse False "write-back.enable" meta) "write-back.enable" docMeta
  when writeBack $ writeToMarkdownFile path pandoc
  return pandoc

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

-- | Calculates paths relative to docBase for all runtime paths contained in
-- the meta data. Also calls need on those files.
calcRelativeResourcePaths :: FilePath -> Pandoc -> Action Pandoc
calcRelativeResourcePaths base (Pandoc meta content) = do
  calculated <- needMetaTargets base meta
  return (Pandoc calculated content)

-- |  The old style decker filter pipeline.
deckerPipeline (Disposition Deck Html) =
  concatM
    [ evaluateShortLinks,
      -- expandTemplateMacros,
      expandDeckerMacros,
      processDetailDiv,
      processSlides,
      handlePolls,
      handleQuizzes
    ]
deckerPipeline (Disposition Page Html) =
  concatM
    [ evaluateShortLinks,
      -- expandTemplateMacros,
      expandDeckerMacros,
      processDetailDiv,
      processDetailHeader
    ]
deckerPipeline (Disposition Index Html) =
  concatM
    [ evaluateShortLinks,
      -- expandTemplateMacros,
      expandDeckerMacros,
      processDetailDiv,
      processDetailHeader
    ]
deckerPipeline (Disposition Handout Html) =
  concatM
    [ evaluateShortLinks,
      -- expandTemplateMacros,
      expandDeckerMacros,
      processDetailDiv,
      processSlides
    ]
deckerPipeline disp = error $ "Disposition not supported: " <> show disp

writeToMarkdown :: Pandoc -> IO Text
writeToMarkdown pandoc@(Pandoc pmeta _) = do
  template <-
    compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n\n$body$"
      >>= handleLeftM

  let columns = lookupMetaOrElse 80 "write-back.line-columns" pmeta
  let wrapOpt :: Text -> WrapOption
      wrapOpt "none" = WrapNone
      wrapOpt "preserve" = WrapPreserve
      wrapOpt _ = WrapAuto
  let wrap = lookupMetaOrElse "auto" "write-back.line-wrap" pmeta
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
  runIO (writeMarkdown options pandoc) >>= handleError

-- | Writes a pandoc document atomically to a markdown file.
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc@(Pandoc pmeta _) = do
  putNormal $ "# write back markdown (" <> filepath <> ")"
  markdown <- liftIO $ writeToMarkdown pandoc
  fileContent <- liftIO $ Text.readFile filepath
  when (markdown /= fileContent)
    $ withTempFile
      (\tmp -> liftIO $ Text.writeFile tmp markdown >> Dir.renameFile tmp filepath)

formatStdin :: IO ()
formatStdin = do
  Text.getContents >>= parseMarkdown >>= writeToMarkdown >>= Text.putStr
