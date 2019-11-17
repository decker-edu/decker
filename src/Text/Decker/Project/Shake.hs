{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Project.Shake
  ( runDecker
  , allHtmlA
  , allPdfA
  , appDataA
  , cacheA
  , calcSource
  , calcSource'
  , currentlyServedPages
  , decksA
  , decksPdfA
  , getRelativeSupportDir
  , handoutsA
  , handoutsPdfA
  , loggingA
  , metaA
  , globalMetaA
  , indicesA
  , openBrowser
  , pagesA
  , pagesPdfA
  , projectA
  , projectDirsA
  , publicA
  , publicResourceA
  , putCurrentDocument
  , runHttpServer
  , startHttpServer
  , stopHttpServer
  , supportA
  , targetsA
  , staticA
  , watchChangesAndRepeat
  , writeDeckIndex
  , writeSketchPadIndex
  , writeSupportFilesToPublic
  , withShakeLock
  , getTemplate
  , getTemplate'
  , isDevRun
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Git
import Text.Decker.Project.Project
import Text.Decker.Project.Version
import Text.Decker.Resource.Template
import Text.Decker.Resource.Zip
import Text.Decker.Server.Server
import Text.Pandoc.Lens as P

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson as Json
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Typeable
import Data.Yaml as Yaml
import Development.Shake hiding (doesDirectoryExist)
import Development.Shake as Shake
  ( Action
  , Resource
  , Rules
  , ShakeOptions(..)
  , getShakeOptions
  , liftIO
  , newResourceIO
  , shakeArgs
  , shakeOptions
  , withResource
  )
import System.Directory as Dir
import qualified System.FSNotify as Notify
import System.FilePath
import System.Info
import System.Process
import Text.Pandoc hiding (Template)
import Text.Pandoc.Shared
import Text.Pandoc.Walk

instance Show (IORef a) where
  show _ = "IORef"

data MutableActionState = MutableActionState
  { _devRun :: Bool
  , _server :: IORef (Maybe Server)
  , _watch :: IORef Bool
  , _publicResource :: Shake.Resource
  } deriving (Show)

makeLenses ''MutableActionState

data ActionContext = ActionContext
  { _dirs :: ProjectDirs
  , _targetList :: Targets
  , _metaData :: Meta
  , _globalMeta :: Meta
  , _state :: MutableActionState
  , _templates :: [(FilePath, Template)]
  } deriving (Typeable, Show)

makeLenses ''ActionContext

initMutableActionState = do
  devRun <- isDevelopmentRun
  server <- newIORef Nothing
  watch <- newIORef False
  public <- newResourceIO "public" 1
  return $ MutableActionState devRun server watch public

runDecker :: Rules () -> IO ()
runDecker rules = do
  state <- initMutableActionState
  catch (repeatIfTrue $ runShakeOnce state rules) (putError "Terminated: ")
  cleanup state

runShakeOnce :: MutableActionState -> Rules () -> IO Bool
runShakeOnce state rules = do
  context <- initContext state
  options <- deckerShakeOptions context
  catch (shakeArgs options rules) (putError "Error: ")
  server <- readIORef (state ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (state ^. watch)
  when keepWatching $ do
    let projectDir = context ^. dirs . project
    let exclude = map (projectDir </>) $ excludeDirs (context ^. metaData)
    -- inDirs <- fastGlobDirs exclude (context ^. dirs . project)
    -- waitForChange inDirs
    waitForChange' projectDir exclude
  return keepWatching

targetDirs context =
  unique $ map takeDirectory (context ^. targetList . sources)

initContext state = do
  dirs <- projectDirectories
  meta <- readMetaData $ dirs ^. project
  targets <- scanTargets meta dirs
  templates <- readTemplates (dirs ^. project) (state ^. devRun)
  -- init context with 2x meta (one will be fixed global meta)
  return $ ActionContext dirs targets meta meta state templates

cleanup state = do
  srvr <- readIORef $ state ^. server
  forM_ srvr stopHttpServer

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  ref <- _watch . _state <$> actionContext
  liftIO $ writeIORef ref True

putError :: String -> SomeException -> IO ()
putError prefix (SomeException e) = putStrLn $ prefix ++ show e

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  cores <- getNumCapabilities
  return $
    shakeOptions
      { shakeFiles = ".shake"
      , shakeColor = True
      , shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty
      , shakeThreads = cores
      , shakeAbbreviations =
          [ (ctx ^. dirs . project ++ "/", "")
          , (ctx ^. dirs . public ++ "/", "")
          ]
      }

actionContextKey :: TypeRep
actionContextKey = typeOf (undefined :: ActionContext)

actionContext :: Action ActionContext
actionContext = do
  options <- getShakeOptions
  let extra = shakeExtra options
  let dyn =
        fromMaybe
          (error "Error looking up action context")
          (HashMap.lookup actionContextKey extra)
  return $ fromMaybe (error "Error upcasting action context") (fromDynamic dyn)

waitForChange :: [FilePath] -> IO ()
waitForChange inDirs =
  Notify.withManager
    (\manager -> do
       done <- newEmptyMVar
       forM_
         inDirs
         (\dir
            -- putStrLn $ "watching dir: " ++ dir
           -> do
            Notify.watchDir
              manager
              dir
              (const True)
              (\e
                 -- putStrLn $ "changed: " ++ show e
                -> do putMVar done ()))
       takeMVar done)

waitForChange' :: FilePath -> [FilePath] -> IO ()
waitForChange' inDir exclude =
  Notify.withManager
    (\manager -> do
       done <- newEmptyMVar
       -- putStrLn $ "watching dir: " ++ inDir
       Notify.watchTree
         manager
         inDir
         filter
         (\e
            -- putStrLn $ "changed: " ++ show e
           -> do putMVar done ())
       takeMVar done)
  where
    filter event = not $ any (`isPrefixOf` (Notify.eventPath event)) exclude

getTemplate :: Disposition -> Action String
getTemplate disposition = getTemplate' (templateFileName disposition)

-- | Get a template by path either from the embedded ZIP archive or from the
-- file system. In the later case, call need on the template file.
getTemplate' :: FilePath -> Action String
getTemplate' path = do
  context <- actionContext
  let (Template content source) = fromJust $ lookup path (context ^. templates)
  case source of
    Just file -> need [file]
    Nothing -> pure ()
  return $ BC.unpack content

isDevRun :: Action Bool
isDevRun = do
  context <- actionContext
  return (context ^. state . devRun)

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from = do
  sup <- _support . _dirs <$> actionContext
  return $ makeRelativeTo from sup

sketchPadId :: T.Text -> T.Text
sketchPadId text =
  T.take 9 $ decodeUtf8 $ B16.encode $ md5DigestBytes $ md5 $ BL.fromStrict $
  encodeUtf8 text

writeSupportFilesToPublic :: Action ()
writeSupportFilesToPublic = do
  correct <- correctSupportInstalled
  unless correct $ do
    removeSupport
    extractSupport
  copyStaticDirs

copyStaticDirs :: Action ()
copyStaticDirs = do
  meta <- metaA
  public <- publicA
  project <- projectA
  let staticSrc = map (project </>) (staticDirs meta)
  let staticDst = map ((public </>) . stripParentPrefix) (staticDirs meta)
  liftIO $ zipWithM_ copyDir staticSrc staticDst
  where
    stripParentPrefix :: FilePath -> FilePath
    stripParentPrefix path =
      if "../" `isPrefixOf` path
        then stripParentPrefix (drop 3 path)
        else path

extractSupport :: Action ()
extractSupport = do
  context <- actionContext
  let publicDir = context ^. dirs . public
  let supportDir = context ^. dirs . support
  liftIO $ do
    extractResourceEntries "support" publicDir
    BC.writeFile (supportDir </> ".version") (BC.pack deckerGitCommitId)

correctSupportInstalled :: Action Bool
correctSupportInstalled = do
  context <- actionContext
  let supportDir = context ^. dirs . support
  liftIO $ handle (\(SomeException _) -> return False) $ do
    supportCommitId <- B.readFile (supportDir </> ".version")
    return $ supportCommitId == BC.pack deckerGitCommitId

removeSupport :: Action ()
removeSupport = do
  context <- actionContext
  let supportDir = context ^. dirs . support
  liftIO $ handle (\(SomeException _) -> return ()) $
    removeDirectoryRecursive supportDir

writeDeckIndex :: FilePath -> FilePath -> Pandoc -> Action Pandoc
writeDeckIndex markdownFile out pandoc@(Pandoc meta _) = do
  let generateIds = getMetaBoolOrElse "generate-ids" False meta
  if not generateIds
    then return pandoc
    else writeDeckIndex' markdownFile out pandoc

writeDeckIndex' :: FilePath -> FilePath -> Pandoc -> Action Pandoc
writeDeckIndex' markdownFile out pandoc@(Pandoc meta _) = do
  putNormal $ "# write deck index (" ++ out ++ ")"
  context <- actionContext
  branch <- liftIO $ textFromMaybe <$> gitBranch
  commit <- liftIO $ textFromMaybe <$> gitRevision
  gitUrl <- liftIO $ textFromMaybe <$> gitOriginUrl
  let repoId = sketchPadId gitUrl
  let proj = context ^. dirs . project
  let publ = context ^. dirs . public
  let title = getMetaStringOrElse "title" "" meta
  let subtitle = getMetaStringOrElse "subtitle" "" meta
  let indexUrl = T.pack $ "/" </> makeRelative publ out
  let sourceDir = T.pack $ makeRelative proj $ takeDirectory markdownFile
  let sourceFile = T.pack $ makeRelative proj markdownFile
  let deckId = sketchPadId sourceFile
  let slideObject i t =
        object
          [ ("id", String $ T.strip $ T.pack i)
          , ("title", String $ T.strip $ T.pack t)
          ]
  let slides = [slideObject i t | (i, t) <- query headers pandoc]
  let fixTitleId slides =
        if title == ""
          then slides
          else slideObject "decker-title-slide" title : slides
  let yaml =
        object
          [ ("commit-id", String commit)
          , ("branch", String branch)
          , ("index-url", String indexUrl)
          , ("repository-url", String gitUrl)
          , ("repository-id", String repoId)
          , ("source-directory", String sourceDir)
          , ("source-file", String sourceFile)
          , ("deck-id", String deckId)
          , ("title", String $ T.pack title)
          , ("subtitle", String $ T.pack subtitle)
          , ("slides", array $ fixTitleId slides)
          ]
  liftIO $ Yaml.encodeFile out yaml
  liftIO $ Json.encodeFile (out -<.> "json") yaml
  return $ injectIds deckId repoId pandoc
  where
    headers (Header 1 (id@(_:_), _, _) text) = [(id, stringify text)]
    headers _ = []
    injectIds :: T.Text -> T.Text -> Pandoc -> Pandoc
    injectIds deckId repoId (Pandoc meta blocks) =
      Pandoc
        (addMetaField "sketch-pad-deck-id" (T.unpack deckId) $
         addMetaField "sketch-pad-repository-id" (T.unpack repoId) meta)
        blocks

textFromMaybe = T.strip . T.pack . fromMaybe "<empty>"

writeSketchPadIndex :: FilePath -> [FilePath] -> Action ()
writeSketchPadIndex out indexFiles = do
  putNormal $ "# write sketch-pad index (" ++ out ++ ")"
  context <- actionContext
  branch <- liftIO $ textFromMaybe <$> gitBranch
  commit <- liftIO $ textFromMaybe <$> gitRevision
  gitUrl <- liftIO $ textFromMaybe <$> gitOriginUrl
  let repoId = sketchPadId gitUrl
  let proj = context ^. dirs . project
  let publ = context ^. dirs . public
  decks <-
    liftIO $ catMaybes <$>
    mapM (analyseDeckIndex (takeDirectory out)) indexFiles
  let yaml =
        object
          [ ("commit-id", String commit)
          , ("branch", String branch)
          , ("repository-url", String gitUrl)
          , ("repository-id", String repoId)
          , ("decks", array decks)
          ]
  liftIO $ Yaml.encodeFile out yaml
  liftIO $ Json.encodeFile (out -<.> "json") yaml

deckEntry :: FilePath -> T.Text -> T.Text -> T.Text -> Yaml.Value
deckEntry path title subtitle did =
  object
    [ ("path", String $ T.pack path)
    , ("title", String title)
    , ("subtitle", String subtitle)
    , ("deck-id", String did)
    ]

analyseDeckIndex :: FilePath -> FilePath -> IO (Maybe Yaml.Value)
analyseDeckIndex relDir indexFile = do
  result <-
    Yaml.decodeFileEither indexFile :: IO (Either Yaml.ParseException Yaml.Value)
  return $
    case result of
      Right yaml -> do
        let slides = yaml ^. key "slides" . _Array
        if not (null slides)
          then Just $
               deckEntry
                 (makeRelative relDir indexFile)
                 (yaml ^. key "title" . _String)
                 (yaml ^. key "subtitle" . _String)
                 (yaml ^. key "deck-id" . _String)
          else Nothing
      Left e -> error $ "No fucking luck: " ++ show e ++ indexFile

publicResourceA = _publicResource . _state <$> actionContext

projectDirsA :: Action ProjectDirs
projectDirsA = _dirs <$> actionContext

projectA :: Action FilePath
projectA = _project <$> projectDirsA

publicA :: Action FilePath
publicA = _public <$> projectDirsA

cacheA :: Action FilePath
cacheA = _cache <$> projectDirsA

supportA :: Action FilePath
supportA = _support <$> projectDirsA

appDataA :: Action FilePath
appDataA = _appData <$> projectDirsA

loggingA :: Action FilePath
loggingA = _logging <$> projectDirsA

targetsA :: Action Targets
targetsA = _targetList <$> actionContext

metaA :: Action Meta
metaA = _metaData <$> actionContext

-- | Global meta data for this directory from decker.yaml and specified additional files
globalMetaA :: Action Meta
globalMetaA = (_globalMeta <$> actionContext) >>= getAdditionalMeta

indicesA = _indices <$> targetsA

staticA :: Action [FilePath]
staticA = _static <$> targetsA

decksA :: Action [FilePath]
decksA = _decks <$> targetsA

decksPdfA :: Action [FilePath]
decksPdfA = _decksPdf <$> targetsA

pagesA :: Action [FilePath]
pagesA = _pages <$> targetsA

pagesPdfA :: Action [FilePath]
pagesPdfA = _pagesPdf <$> targetsA

handoutsA :: Action [FilePath]
handoutsA = _handouts <$> targetsA

handoutsPdfA :: Action [FilePath]
handoutsPdfA = _handoutsPdf <$> targetsA

mapTargets :: [(Targets -> [FilePath])] -> Action [FilePath]
mapTargets fs = do
  ts <- targetsA
  return $ concatMap ($ ts) fs

allHtmlA :: Action [FilePath]
allHtmlA = mapTargets [_decks, _pages, _handouts]

allPdfA :: Action [FilePath]
allPdfA = mapTargets [_decksPdf, _pagesPdf, _handoutsPdf]

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  r <- _publicResource . _state <$> actionContext
  withResource r 1 perform

-- | Runs the built-in server on the given directory, if it is not already
-- running.
runHttpServer :: Int -> ProjectDirs -> Maybe String -> Action ()
runHttpServer port dirs url = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- liftIO $ startHttpServer dirs port
      liftIO $ writeIORef ref $ Just httpServer
      forM_ url openBrowser

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just (_, state) -> do
      (_, pages) <- liftIO $ readMVar state
      return $ Set.toList pages
    Nothing -> return []

openBrowser :: String -> Action ()
openBrowser url =
  if | any (`isInfixOf` os) ["linux", "bsd"] ->
       liftIO $ callProcess "xdg-open" [url]
     | "darwin" `isInfixOf` os -> liftIO $ callProcess "open" [url]
     | otherwise ->
       putNormal $ "Unable to open browser on this platform for url: " ++ url

reloadBrowsers :: Action ()
reloadBrowsers = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just serv -> liftIO $ reloadClients serv
    Nothing -> return ()

calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target = do
  dirs <- projectDirsA
  let src =
        (replaceSuffix targetSuffix srcSuffix . combine (dirs ^. project) .
         makeRelative (dirs ^. public))
          target
  need [src]
  return src

-- |  calcSource without the call to need and without the suffix replacement
calcSource' :: FilePath -> Action FilePath
calcSource' target = do
  dirs <- projectDirsA
  return $ dirs ^. project </> makeRelative (dirs ^. public) target

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = do
  public <- publicA
  let rel = makeRelative public out
  putNormal $ "# pandoc (for " ++ rel ++ ")"
