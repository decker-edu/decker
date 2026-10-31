{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Text.Decker.Reader.Markdown
  ( readAndFilterMarkdownFile,
    readMarkdownFile,
    generateChattyMarkdown,
    processCites,
    formatStdin,
  )
where

import Control.Monad
import Control.Monad.Loops
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Development.Shake hiding (Resource)
import Relude
import System.AtomicWrite.Writer.ByteString
import System.Directory qualified as Dir
import System.FilePath.Posix
import Text.Decker.Exam.Filter
import Text.Decker.Filter.Decker2
import Text.Decker.Filter.Detail
import Text.Decker.Filter.Filter
import Text.Decker.Filter.FragmentTemplate (expandFragmentTemplates)
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Paths
import Text.Decker.Filter.Poll
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Select (dropSolutionContent, filterSelectedSlides)
import Text.Decker.Filter.ShortLink
import Text.Decker.Filter.Template (expandTemplateMacros)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Diagnostics
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.MetaExtra (expandMeta, mergeDocumentMeta, needMetaTargets)
import Text.Decker.Internal.URI
import Text.Decker.Resource.Resource
import Text.Decker.Writer.CSS (computeCssColorVariables, computeCssVariables)
import Text.Pandoc hiding (lookupMeta, report)
import Text.Pandoc.Citeproc
import Text.Pandoc.Shared
import Text.Pandoc.Walk (query, walk)

-- | Reads a Markdown file and run all the the Decker specific filters on it.
-- The path is assumed to be an absolute path in the local file system under
-- the project root directory. Throws an exception if something goes wrong
readAndFilterMarkdownFile :: Disposition -> Meta -> FilePath -> Action Pandoc
readAndFilterMarkdownFile disp globalMeta docPath = do
  let docBase = takeDirectory docPath
  readMarkdownFile globalMeta docPath
    >>= mergeDocumentMeta globalMeta
    >>= processMeta
    >>= processCites docPath
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
  putVerbose "processMeta"
  let processed = computeCssColorVariables $ computeCssVariables meta
  return (Pandoc processed blocks)

-- | Provide default CSL data from the resources if csl: is not set. Citeproc
-- warnings (e.g. unresolved keys) are reported as diagnostics for @docPath@.
-- Without a bibliography citations are left alone, which is reported, too.
processCites :: FilePath -> Pandoc -> Action Pandoc
processCites docPath pandoc@(Pandoc meta blocks) = do
  putVerbose "processCites"
  if
    | isMetaSet "bibliography" meta && isMetaSet "csl" meta ->
        citeproc pandoc
    | isMetaSet "bibliography" meta -> do
        defaultCSL <- liftIO $ installDefaultCSL meta
        let cslMeta = setMetaValue "csl" defaultCSL meta
        citeproc (Pandoc cslMeta blocks)
    | otherwise -> do
        let keys = ordNub $ query bracketedCitationKeys pandoc
        unless (null keys)
          $ report meta
          $ warning
            "citation-without-bibliography"
            docPath
            Nothing
            ( "citation "
                <> Text.intercalate ", " keys
                <> " rendered literally: no bibliography is configured"
            )
        return pandoc
  where
    citeproc document = do
      result <- liftIO $ runPandocCollecting docPath (processCitations document)
      (processed, diagnostics) <- liftIO $ handleError result
      reportAll meta diagnostics
      return processed
    -- Bare in-text @key is ignored; it is too often meant literally.
    bracketedCitationKeys (Cite citations _) =
      [citationId c | c <- citations, citationMode c /= AuthorInText]
    bracketedCitationKeys _ = []

installDefaultCSL :: Meta -> IO FilePath
installDefaultCSL meta = do
  transient <- transientDir
  let path = transient </> "default.csl"
  exists <- Dir.doesFileExist path
  unless exists $ do
    csl <- readResource' "default.csl" meta
    atomicWriteFile path (fromJust csl)
  return path

-- | Reads a Markdown file from the local file system. Local resource paths are
-- converted to absolute paths. Additional meta data is read and merged into
-- the document. Other Markdown files may be transitively included. Throws an
-- exception if something goes wrong
readMarkdownFile' :: Meta -> FilePath -> FilePath -> Action Pandoc
readMarkdownFile' globalMeta top path = do
  let base = takeDirectory path
  putVerbose "readMarkdownFile"
  parseMarkdownFile globalMeta path
    >>= addDocumentPath globalMeta path
    >>= writeBack globalMeta path
    >>= expandMeta globalMeta base
    >>= adjustResourcePathsA base
    >>= checkVersion
    >>= includeMarkdownFiles globalMeta top base
    >>= addPathInfo base

readMarkdownFile :: Meta -> FilePath -> Action Pandoc
readMarkdownFile globalMeta path = readMarkdownFile' globalMeta path path

-- | (Re)generates the annotated chatty markdown for the given source documents
-- (decks and pages), one file per source. Each source is read with all includes
-- expanded and its meta merged with the global meta, then the publishing filter
-- is applied (dropping solution slides and boxes for upcoming lectures, see
-- 'dropSolutionContent') before the document is written to @chatty\/<source>.md@.
-- Drafts are expected to have been excluded from the input list by the caller.
-- Sources with @chatty.exclude: true@ in their merged metadata are skipped.
-- Used by `decker publish` and `decker chatty` to populate the directory synced
-- to the OpenAI vector store.
generateChattyMarkdown :: Meta -> [FilePath] -> Action ()
generateChattyMarkdown globalMeta = mapM_ generate
  where
    generate path = do
      pandoc <- readMarkdownFile globalMeta path
      Pandoc mergedMeta _ <- mergeDocumentMeta globalMeta pandoc
      unless (lookupMetaOrElse False "chatty.exclude" mergedMeta) $
        writeToMarkdownFile path "chatty" path (dropSolutionContent mergedMeta pandoc)

addDocumentPath :: Meta -> FilePath -> Pandoc -> Action Pandoc
addDocumentPath globalMeta documentPath pandoc@(Pandoc meta blocks) = do
  absDocumentPath <- liftIO $ Dir.makeAbsolute documentPath
  let editor = lookupMetaOrElse "zed://file" "editor.link-prefix" meta
  let documentUrl = toText $ editor ++ absDocumentPath;
  return
    $ if not (lookupMetaOrElse False "lecture.publish" globalMeta)
      && lookupMetaOrElse False "experiments.add-document-path" globalMeta
      then walk (addToHeader1 documentUrl) pandoc
      else pandoc
  where
    addToHeader1 documentUrl (Header 1 (id, cls, kvs) content) =
      Header
        1
        (id, cls, addPath kvs)
        (content <> [Link ("", ["document-url"], [])[Span nullAttr [Space, Str $ toText documentPath]] (documentUrl, "")])
    addToHeader1 _ block = block
    addPath kvs = ("data-source-path", toText documentPath) : kvs

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
-- Pandoc's parser warnings and the 'setextHints' are reported as diagnostics
-- for the file.
parseMarkdownFile :: Meta -> FilePath -> Action Pandoc
parseMarkdownFile globalMeta path = do
  markdown <- liftIO $ Text.readFile path
  reportAll globalMeta (setextHints path markdown)
  result <- liftIO $ runPandocCollecting path (readMarkdown pandocReaderOpts markdown)
  (pandoc, diagnostics) <- liftIO $ handleError result
  reportAll globalMeta diagnostics
  return pandoc

-- | Finds lines of dashes directly below a line of text. Pandoc reads them as
-- a setext level-2 heading, not as a slide break. This is a heuristic (it
-- cannot see everything the parser sees), so it only produces hints. Fenced
-- code, YAML blocks and multiline tables (blocks that start with a dash line)
-- are skipped.
setextHints :: FilePath -> Text -> [Diagnostic]
setextHints path markdown = go Nothing "" (zip [1 ..] (Text.lines markdown))
  where
    -- The state is the closing line of the fenced code or YAML block we are in
    -- (if any) and the previous line.
    go :: Maybe Text -> Text -> [(Int, Text)] -> [Diagnostic]
    go _ _ [] = []
    go (Just close) _ ((_, line) : rest)
      | close `Text.isPrefixOf` Text.strip line = go Nothing "" rest
      | close == "---" && Text.stripEnd line == "..." = go Nothing "" rest
      | otherwise = go (Just close) line rest
    go Nothing previous ((n, line) : rest)
      | Just fence <- openingFence line = go (Just fence) line rest
      | isYamlStart n previous line rest = go (Just "---") line rest
      | isMultilineTableStart previous line rest = skipTable rest
      | isDashLine line && isParagraphText previous =
          hint
            "setext-heading"
            path
            (Just (n - 1))
            ( "the dash line below \""
                <> Text.strip previous
                <> "\" turns it into a level-2 heading; add a blank line before the dashes if a slide break was intended"
            )
            : go Nothing line rest
      | otherwise = go Nothing line rest
    -- A multiline table starts with a dash line after a blank line, directly
    -- followed by text, and ends with a dash line followed by a blank line.
    isMultilineTableStart previous line rest =
      isDashLine line
        && isBlank previous
        && case rest of
          ((_, next) : _) -> not (isBlank next) && not ("#" `Text.isPrefixOf` next)
          [] -> False
    skipTable ((_, line) : rest@((_, next) : _))
      | isDashLine line && isBlank next = go Nothing line rest
      | otherwise = skipTable rest
    skipTable _ = []
    isBlank = Text.null . Text.strip
    openingFence line =
      let stripped = Text.stripStart line
          fence c = Text.takeWhile (== c) stripped
       in listToMaybe [f | c <- ['`', '~'], let f = fence c, Text.length f >= 3]
    isDashLine line = let s = Text.stripEnd line in Text.length s >= 3 && Text.all (== '-') s
    -- Front matter at the top of the file, or a YAML block further down that
    -- starts with a key.
    isYamlStart n previous line rest =
      Text.stripEnd line == "---"
        && isBlank previous
        && case rest of
          ((_, next) : _) -> not (isBlank next) && (n == 1 || looksLikeYamlKey next)
          [] -> False
    looksLikeYamlKey next =
      let (key, colon) = Text.break (== ':') next
       in not (Text.null key)
            && not (Text.null colon)
            && Text.all (\c -> isAlphaNum c || c `elem` ['-', '_', '.']) key
    isParagraphText previous =
      let stripped = Text.strip previous
          first = Text.take 1 stripped
       in not (Text.null stripped)
            && not ("    " `Text.isPrefixOf` previous || "\t" `Text.isPrefixOf` previous)
            && first `notElem` ["#", "|", "+", "-", "*", ":", "<", ">"]
            && not (isOrderedListItem stripped)
    isOrderedListItem text =
      let (digits, rest) = Text.span isDigit text
       in not (Text.null digits) && (Text.take 1 rest `elem` [".", ")"])

-- | Writes a Pandoc document to a file in Markdown format. Throws an exception
-- if something goes wrong
writeBack :: Meta -> FilePath -> Pandoc -> Action Pandoc
writeBack meta path pandoc@(Pandoc docMeta _) = do
  let writeBack :: Bool = lookupMetaOrElse (lookupMetaOrElse False "write-back.enable" meta) "write-back.enable" docMeta
  when writeBack $ writeToMarkdownFile "" "" path pandoc
  return pandoc

checkVersion :: Pandoc -> Action Pandoc
checkVersion = return

-- | Traverses the pandoc AST and transitively embeds included Markdown files.
includeMarkdownFiles :: Meta -> FilePath -> FilePath -> Pandoc -> Action Pandoc
includeMarkdownFiles globalMeta top docBase (Pandoc docMeta content) =
  Pandoc docMeta <$> processBlocks content
  where
    processBlocks :: [Block] -> Action [Block]
    processBlocks blcks = concat . reverse <$> foldM include [] blcks
    include :: [[Block]] -> Block -> Action [[Block]]
    include document (Para [Link _ [Str ":include"] (url, _)]) = do
      let path = makeProjectPath docBase (toString url)
      -- putVerbose $ "# --> include: " <> toString url <> " (" <> path <> ")"
      need [path]
      Pandoc _ includedBlocks <- readMarkdownFile' globalMeta top path
      return $ includedBlocks : document
    include document block = return $ [block] : document

-- | Calculates paths relative to docBase for all runtime paths contained in
-- the meta data. Also calls need on those files.
calcRelativeResourcePaths :: FilePath -> Pandoc -> Action Pandoc
calcRelativeResourcePaths base (Pandoc meta content) = do
  putVerbose "calcRelativeResourcePaths"
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
            . disableExtension Ext_auto_identifiers
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
writeToMarkdownFile :: FilePath -> FilePath -> FilePath -> Pandoc -> Action ()
writeToMarkdownFile top base filepath pandoc@(Pandoc pmeta blocks) = do
  absProjectDir <- liftIO $ Dir.makeAbsolute projectDir
  canonFile <- liftIO $ (makeRelative absProjectDir <$> Dir.canonicalizePath filepath)
  canonTop <- liftIO $ (makeRelative absProjectDir <$> Dir.canonicalizePath top)
  let path = base </> canonFile
  let meta0 = addMetaKeyValue "chatty.filepath" canonFile pmeta
  let meta1 = if canonFile /= canonTop
                then addMetaKeyValue "chatty.url-path" (canonTop -<.> "html") $ addMetaKeyValue "chatty.included-from" top meta0
                else addMetaKeyValue "chatty.url-path" (canonFile -<.> "html") meta0
  liftIO $ Dir.createDirectoryIfMissing True (takeDirectory path)
  putNormal $ "# write back markdown (" <> path <> ")"
  markdown <- liftIO $ writeToMarkdown (Pandoc meta1 (addSlideUrl (toText $ canonTop -<.> "html") blocks))
  writeFileChanged path (toString markdown)
  where
    addSlideUrl deckUrl = walk (add deckUrl)
    add deckUrl (Header 1 (id, cls, kvs) inlines) = Header 1 (id, cls, ("url", deckUrl <> "#" <> id):kvs) inlines
    add deckUrl block = block

formatStdin :: IO ()
formatStdin = do
  let opts =
        def
          { readerExtensions =
              disableExtension Ext_auto_identifiers
                $ disableExtension Ext_implicit_figures
                $ enableExtension Ext_emoji pandocExtensions,
            readerColumns = 999
          }
  Text.getContents >>= parseMarkdown opts >>= writeToMarkdown >>= Text.putStr
  where
    parseMarkdown opts text = runIOorExplode (readMarkdown opts text)
