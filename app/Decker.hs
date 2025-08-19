module Decker where

import Control.Concurrent
import Control.Exception (SomeException (SomeException), catch)
import Control.Lens ((^.))
import Control.Lens qualified as Control.Lens.Getter
import Control.Monad.Extra
import Data.Aeson (encodeFile)
import Data.IORef ()
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.String ()
import Data.Time.Format.ISO8601
import Development.Shake
import GHC.IO.Encoding
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory qualified as Dir
import System.Directory.Extra (getFileSize)
import System.FilePath.Glob qualified as Glob
import System.FilePath.Posix
import System.IO
import Text.Decker.Exam.Question
import Text.Decker.Exam.Render
import Text.Decker.Exam.Xml
import Text.Decker.Filter.Index
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
    ( runExternal, runExternalForSVG )
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.ActionContext (Flags (LectureFlag), actionContext, extra)
import Text.Decker.Project.Glob (fastGlobFiles')
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Zip
import Text.Decker.Writer.Layout
import Text.Groom
import System.Directory (makeRelativeToCurrentDirectory)
import Path (parseRelDir)
import Path.IO (copyDirRecur)

main :: IO ()
main = do
  setLocaleEncoding utf8
  setProjectDirectory
  run

needTargets sel = needTargets' [sel]

needTargets' :: [Control.Lens.Getter.Getting Dependencies Targets Dependencies] -> Targets -> Action ()
needTargets' sels targets = do
  let ts = concatMap (\s -> Map.keys (targets ^. s)) sels
  need ts

needPublicIfExists :: FilePath -> Action ()
needPublicIfExists source = do
  let target = publicDir </> source
  exists <- doesFileExist source
  if exists
    then do
      need [target]
    else do
      removeFileA target

needPublicIfExistsGlob :: FilePath -> Action ()
needPublicIfExistsGlob source = do
  files <- liftIO $ Glob.glob source
  relative <- liftIO $ mapM makeRelativeToCurrentDirectory files
  forM_ relative needPublicIfExists

-- | Remove a file, but don't worry if it fails
removeFileA :: FilePath -> Action ()
removeFileA target = do
  didIt <- liftIO $ (removeFile target >> return True) `catch` \(SomeException e) -> return False
  when didIt $ putNormal $ "# rm (for " <> target <> ")"

serverPort = 8888

serverUrl = "http://localhost:" ++ show serverPort

generatedIndex = publicDir </> "index-generated.html"

indexFile = publicDir </> "index.html"

run :: IO ()
run = do
  runDecker deckerRules

runArgs :: [String] -> IO ()
runArgs args = do
  runDeckerArgs args deckerRules

deckerRules = do
  (getGlobalMeta, getDeps, getTemplate) <- prepCaches
  transient <- liftIO transientDir
  want ["html"]
  addHelpSuffix "Commands:"
  addHelpSuffix "  - clean - Remove all generated files."
  addHelpSuffix "  - purge - Your sins will be forgiven."
  addHelpSuffix "  - example - Create an example project."
  addHelpSuffix "  - serve - Start just the server."
  addHelpSuffix "  - crunch - Compress all recordings to smaller size. Takes a while and will drain your battery."
  addHelpSuffix "  - transcribe - Transcribe recorded videos. Takes a while and will drain your battery."
  addHelpSuffix "  - pdf - Build PDF versions of all decks (*-deck.md)."
  addHelpSuffix "  - version - Print version information"
  addHelpSuffix "  - check - Check the existence of usefull external programs"
  addHelpSuffix "  - format - Format Decker Markdown from stdin to stdout. Use with your favourite text editor."
  addHelpSuffix ""
  addHelpSuffix "For additional information see: https://go.uniwue.de/decker-wiki"
  --
  withTargetDocs "Build HTML versions of all question (*-quest.md)." $
    phony "questions" $ do
      need ["support"]
      getDeps >>= needTargets questions
  --
  withTargetDocs "Build HTML versions of all decks and pages (*-deck.md, *-page.md)." $
    phony "html" $ do
      need ["support", "questions"]
      getDeps >>= needTargets' [decks, pages]
  --
  withTargetDocs "Build HTML versions of all decks (*-deck.md)." $
    phony "decks" $ do
      meta <- getGlobalMeta
      need ["support"]
      getDeps >>= needTargets decks
  --
  withTargetDocs "Build HTML versions of all handouts (*-handout.md)." $
    phony "handouts" $ do
      need ["support", "questions"]
      getDeps >>= needTargets' [handouts]
  --
  withTargetDocs "Build HTML versions of all pages (*-page.md)." $
    phony "pages" $ do
      need ["support", "questions"]
      getDeps >>= needTargets' [pages]
  --
  phony "pdf" $ do
    need ["support"]
    getDeps >>= needTargets decksPdf
  --
  withTargetDocs "Compile global search index." $
    phony "search-index" $ do
      putInfo "# compiling search index ..."
      meta <- getGlobalMeta
      deps <- getDeps
      let deckSrcs = Map.elems $ deps ^. decks
      _ <- buildIndex (publicDir </> "index.json") meta deckSrcs
      return ()

  --
  withTargetDocs "If a tree falls in a forest and no one is there to hear, does it make a sound?" $
    phony "observed" $ do
      watchChangesAndRepeat
      need ["support"]
      pages <- currentlyServedPages
      need $ map (publicDir </>) pages
  --
  -- priority 5 $ do
  --   (publicDir </> "support") <//> "*" %> \out -> do
  --     deps <- getDeps
  --     let path = fromJust $ stripPrefix (publicDir <> "/") out
  --     let source = (deps ^. resources) Map.! out
  --     putVerbose $ "# extract (" <> out <> " from " <> show source <> " : " <> path <> ")"
  --     needResource source path
  --     content <- fromJust <$> liftIO (readResource path source)
  --     liftIO $ BS.writeFile out content
  --
  --
  priority 5 $ do
    (supportDir </> deckerGitCommitId) %> \out -> do
      meta <- getGlobalMeta
      liftIO $ do
        createDirectoryIfMissing True supportDir
        touchFile out
        (Resources dr pr) <- deckerResources meta
        extractFast dr
        extractFast pr
  priority 4 $ do
    publicDir <//> "*-deck.html" %> \out -> do
      src <- lookupSource decks out <$> getDeps
      need [src]
      meta <- getGlobalMeta
      markdownToHtml htmlDeck meta getTemplate src out
      needPublicIfExists $ replaceSuffix "-deck.md" "-annot.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-manip.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-times.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-transcript.json" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-recording.mp4" src
      needPublicIfExists $ replaceSuffix "-deck.md" "-recording.vtt" src
      needPublicIfExistsGlob $ replaceSuffix "-deck.md" "-recording-*.vtt" src
    --
    publicDir <//> "*-deck.pdf" %> \out -> do
      let src = replaceSuffix "-deck.pdf" "-deck.html" out
      let annot = replaceSuffix "-deck.pdf" "-annot.json" $ makeRelative publicDir out
      -- This is the right way to depend on an optional file. Just check for the
      -- files existence with the Shake function `doesFileExist`.
      exists <- doesFileExist annot
      when exists $ need [annot]
      need [src]
      let url = serverUrl </> makeRelative publicDir src 
      putInfo $ "# chrome started ... (for " <> out <> ")"
      meta <- getGlobalMeta
      liftIO $ runExternal "chrome" url out meta
      putInfo $ "# chrome finished (for " <> out <> ")"
    --
    publicDir <//> "*-handout.html" %> \out -> do
      src <- lookupSource handouts out <$> getDeps
      need [src]
      meta <- getGlobalMeta
      markdownToHtml htmlHandout meta getTemplate src out
    --
    publicDir <//> "*-page.html" %> \out -> do
      src <- lookupSource pages out <$> getDeps
      need [src]
      meta <- getGlobalMeta
      markdownToHtml htmlPage meta getTemplate src out
    --
    publicDir <//> "*.css" %> \out -> do
      let src = makeRelative publicDir out
      putVerbose $ "# copy (for " <> out <> ")"
      copyFile' src out
      whenM (liftIO $ Dir.doesFileExist (src <.> "map")) $
        copyFile' (src <.> "map") (out <.> "map")
    --
    privateDir <//> "*-quest.html" %> \out -> do
      src <- lookupSource questions out <$> getDeps
      need [src]
      meta <- getGlobalMeta
      renderQuestion meta src out
    --
    privateDir <//> "quest-catalog.html" %> \out -> do
      meta <- getGlobalMeta
      deps <- getDeps
      let sources = Map.elems (deps ^. questions)
      need sources
      renderCatalog meta sources out
    --
    privateDir <//> "quest-catalog.xml" %> \out -> do
      deps <- getDeps
      let sources = Map.elems (deps ^. questions)
      need sources
      questions <- liftIO $ mapM readQuestion sources
      renderXmlCatalog questions out
    --
    phony "catalog" $ do
      need ["private/quest-catalog.html"]
    --
    phony "moodle-xml" $ do
      need ["private/quest-catalog.xml"]
    --
    indexFile %> \out -> do
      meta <- getGlobalMeta
      deps <- getDeps
      exists <- doesFileExist indexSource
      if exists
        then do
          need [indexSource]
          targetMeta <- addTargetInfo deps meta
          markdownToHtml htmlIndex targetMeta getTemplate indexSource out
          template <- getTemplate "template/index-generated.html"
          renderIndex template meta deps generatedIndex
        else do
          template <- getTemplate "template/index-generated.html"
          renderIndex template meta deps out
  --
  priority 3 $ do
    "**/*.css" %> \out -> do
      let src = out -<.> "scss"
      whenM (liftIO $ Dir.doesFileExist src) $ do
        need [src]
        putInfo $ "# sassc (for " <> out <> ")"
        -- command [] "sassc" [src, out]
        meta <- getGlobalMeta
        liftIO $ runExternalForSVG "sassc" src out meta
    --
    "**/*.plantuml.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# plantuml (for " <> out <> ")"
      meta <- getGlobalMeta
      liftIO $ runExternalForSVG "plantuml" src out meta
      -- liftIO $ Dir.renameFile (src -<.> "svg") out
    --
    "**/*.mmd.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# mermaid (for " <> out <> ")"
      -- mermaid ["-i", src, "-o", out] (Just out)
      meta <- getGlobalMeta
      liftIO $ runExternalForSVG "mermaid" src out meta
    --
    "**/*.dot.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# dot (for " <> out <> ")"
      -- dot ["-o" ++ out, src] (Just out)
      meta <- getGlobalMeta
      liftIO $ runExternalForSVG "dot" src out meta
    --
    "**/*.gnuplot.svg" %> \out -> do
      let src = dropExtension out
      need [src]
      putInfo $ "# gnuplot (for " <> out <> ")"
      -- gnuplot ["-e", "\"set output '" ++ out ++ "'\"", src] (Just out)
      meta <- getGlobalMeta
      liftIO $ runExternalForSVG "gnuplot" src out meta
    --
    "**/*.tex.svg" %> \out -> do
      let src = dropExtension out
      let pdf = src -<.> ".pdf"
      let dir = takeDirectory src
      need [src]
      -- pdflatex ["-output-directory", dir, src] Nothing
      -- pdf2svg [pdf, out] (Just out)
      meta <- getGlobalMeta
      liftIO $ runExternal "pdflatex" src dir meta
      liftIO $ runExternalForSVG "pdf2svg" pdf out meta
      liftIO (Dir.removeFile pdf `catch` (\(SomeException _) -> return ()))
  --
  -- Catch all. Just copy project/* to public/*. This nicely handles ALL
  -- resources. Just `need` them where you need them.
  priority 2 $
    publicDir <//> "//" %> \out -> do
      let src = makeRelative publicDir out
      putVerbose $ "# copy (for " <> out <> ")"
      copyFile' src out
  --
  withTargetDocs "Copy static file to public dir." $
    phony "static-files" $ do
      deps <- getDeps
      need $ Map.keys (deps ^. static)
  --
  withTargetDocs "Provide information about project parameters, sources and targets" $
    phony "info" $ do
      project <- liftIO $ Dir.canonicalizePath projectDir
      meta <- getGlobalMeta
      deps <- getDeps
      resources <- liftIO $ deckerResources meta
      liftIO $ do
        putStrLn $ "\nproject directory: " ++ project
        putStrLn $ "public directory: " ++ publicDir
        putStrLn $ "support directory: " ++ supportDir
        putStrLn $ "transient directory: " ++ transient
        putStrLn $ "template source: " <> show resources
  --
  withTargetDocs "Provide information about project parameters, sources and targets" $
    phony "more-info" $ do
      need ["info"]
      meta <- getGlobalMeta
      liftIO $ do
        putStrLn "\ntop level meta data:\n"
        putStrLn (groom meta)
  --
  withTargetDocs "Provide information about project parameters, sources and targets" $
    phony "even-more-info" $ do
      need ["more-info"]
      deps <- getDeps
      liftIO $ do
        putStrLn "\ndependencies:\n"
        putStrLn (groom deps)
  --
  withTargetDocs "Copy runtime support files to public dir." $
    phony "support" $ do
      deps <- getDeps
      need [indexFile, "static-files"]
      -- Resources and their locations are now recorded in deps. Well, no more.
      -- Now use a version file containing the commit hash.
      -- need $ Map.keys (deps ^. resources)
      -- putNormal $ "needing: " <> (supportDir </> deckerGitCommitId)
      need [supportDir </> deckerGitCommitId]
  --
  withTargetDocs "Publish the public dir to the configured destination using rsync." $
    phony "publish" $ do
      meta <- getGlobalMeta
      context <- actionContext
      let flags = context ^. extra
      case lookupMeta "publish.rsync.destination" meta of
        Just (destination :: String) -> do
          if LectureFlag `elem` flags
            then do
              -- clean out the public dir
              liftIO $ runClean False
              -- includes index and static resources
              need ["support"]
              deps <- getDeps
              let deckSrcs = Map.elems $ deps ^. decks
              let pageSrcs = Map.elems $ deps ^. pages
              selected <- buildIndex (publicDir </> "index.json") meta (deckSrcs <> pageSrcs)
              let decks = calcTargets deckSuffix deckHTMLSuffix selected
              -- let decksPdf = calcTargets deckSuffix deckPDFSuffix selected
              let pages' = calcTargets pageSuffix pageHTMLSuffix selected
              need (Map.keys decks <> Map.keys pages')
              -- need (Map.keys decks <> Map.keys decksPdf <> Map.keys pages')
              createPublicManifest
              let src = publicDir ++ "/"
              liftIO $ runExternal "rsync" src destination meta
            else do
              need ["support"]
              getDeps >>= needTargets' [decks, pages]
              createPublicManifest
              let src = publicDir ++ "/"
              liftIO $ runExternal "rsync" src destination meta
        Nothing -> putError "publish.rsync.destination not configured"

createPublicManifest :: Action ()
createPublicManifest = do
  let manifestPath = publicDir <> "/" <> "manifest.json"
  putNormal $ "# writing manifest (to " <> manifestPath <> ")"
  liftIO $ writeFile manifestPath "" -- make sure manifest.json is listed in the manifest
  liftIO $ createDirectoryIfMissing True publicDir
  allFiles <- liftIO $ fastGlobFiles' [] (const True) publicDir
  allFilesWithMeta <- Map.fromList <$> mapM readMeta allFiles
  liftIO $ encodeFile manifestPath allFilesWithMeta
  where
    readMeta file = do
      modTime <- liftIO $ Dir.getModificationTime file
      size <- liftIO $ getFileSize file
      return (stripPublic file, (formatShow iso8601Format modTime, size))
    stripPublic path = fromMaybe path $ stripPrefix "public/" path

waitForYes :: IO ()
waitForYes = do
  threadDelay 1000
  putStr "\nDecker server running. Push ENTER to terminate."
  hFlush stdout
  _ <- getLine
  putStr "Terminate server? (y/N): "
  hFlush stdout
  input <- getLine
  unless (input == "y") waitForYes

extractFast (DeckerExecutable path) = do
  putStrLn $ "extractFast: extracting from executable: " <> path
  extractResourceEntries (path </> "support") supportDir  
extractFast (LocalDir path) = do
  putStrLn $ "extractFast: extracting from local dir: " <> path
  from <- parseRelDir (path </> "support")
  to <- parseRelDir supportDir
  copyDirRecur from to  
extractFast source = putStrLn $ "extractFast: saw: " <> show source  
