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
  , templateSourceA
  , staticA
  , watchChangesAndRepeat
  , writeSupportFilesToPublic
  , withShakeLock
  , getTemplate
  , getTemplate'
  , isDevRun
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Version
import Text.Decker.Resource.Template
import Text.Decker.Server.Server

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Digest.Pure.MD5
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Typeable
import Development.Shake hiding (doesDirectoryExist, putError)
import System.Console.GetOpt
import System.Directory as Dir
import qualified System.FSNotify as Notify
import System.FilePath
import System.Info
import System.Process
import Text.Pandoc hiding (getTemplate)

instance Show (IORef a) where
  show _ = "IORef"

data MutableActionState = MutableActionState
  { _devRun :: Bool
  , _server :: IORef (Maybe Server)
  , _watch :: IORef Bool
  , _publicResource :: Development.Shake.Resource
  } deriving (Show)

makeLenses ''MutableActionState

data ActionContext = ActionContext
  { _dirs :: ProjectDirs
  , _targetList :: Targets
  , _metaData :: Meta
  , _globalMeta :: Meta
  , _state :: MutableActionState
  , _templateSource :: TemplateSource
  , _templates :: [(FilePath, DeckerTemplate)]
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

data Flags
  = MetaValueFlag String
                  String
  | FormatFlag
  deriving (Eq, Show)

deckerFlags :: [OptDescr (Either String Flags)]
deckerFlags =
  [ Option
      ['m']
      ["meta"]
      (ReqArg parseMetaValueArg "META")
      "Set a meta value like this: 'name=value'. Overrides values from decker.yaml."
  , Option
      ['f']
      ["format"]
      (NoArg $ Right FormatFlag)
      "Format Markdown on stdin to stdout"
  ]

parseMetaValueArg :: String -> Either String Flags
parseMetaValueArg arg =
  case splitOn "=" arg of
    [meta, value]
      | isMetaName meta -> Right $ MetaValueFlag meta value
    _ -> Left "Cannot parse argument. Must be 'name=value'."

isMetaName :: String -> Bool
isMetaName str = all check $ splitOn "." str
  where
    check s = length s > 1 && isAlpha (head s) && all isAlphaNum (tail s)

-- TODO: Handle the meta flag
handleArguments :: Rules () -> [Flags] -> [String] -> IO (Maybe (Rules ()))
handleArguments rules flags targets =
  return $ Just $
  if null targets
    then rules
    else want targets >> withoutActions rules

runShakeOnce :: MutableActionState -> Rules () -> IO Bool
runShakeOnce state rules = do
  context <- initContext state
  options <- deckerShakeOptions context
  catch
    (shakeArgsWith options deckerFlags (handleArguments rules))
    (putError "Error: ")
  server <- readIORef (state ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (state ^. watch)
  when keepWatching $ do
    let projectDir = context ^. dirs . project
    let exclude = map (projectDir </>) $ excludeDirs (context ^. metaData)
    waitForChange' projectDir exclude
  return keepWatching

targetDirs context =
  unique $ map takeDirectory (context ^. targetList . sources)

initContext :: MutableActionState -> IO ActionContext
initContext state = do
  dirs <- projectDirectories
  meta <- readMetaData $ dirs ^. project
  targets <- scanTargets meta dirs
  let templateSource =
        calcTemplateSource
          (getMetaText "template-source" meta)
          (state ^. devRun)
  templates <- readTemplates templateSource
  -- init context with 2x meta (one will be fixed global meta)
  return $ ActionContext dirs targets meta meta state templateSource templates

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
         (\dir ->
            Notify.watchDir manager dir (const True) (\e -> putMVar done ()))
       takeMVar done)

waitForChange' :: FilePath -> [FilePath] -> IO ()
waitForChange' inDir exclude =
  Notify.withManager
    (\manager -> do
       done <- newEmptyMVar
       Notify.watchTree manager inDir filter (\e -> putMVar done ())
       takeMVar done)
  where
    filter event = not $ any (`isPrefixOf` Notify.eventPath event) exclude

getTemplate :: Disposition -> Action (Template T.Text)
getTemplate disposition = getTemplate' (templateFileName disposition)

-- | Get a template by path either from the embedded ZIP archive or from the
-- file system. In the later case, call need on the template file.
--
-- TODO: Add proper error handling
--
getTemplate' :: FilePath -> Action (Template T.Text)
getTemplate' path = do
  context <- actionContext
  let (DeckerTemplate template source) =
        fromJust $ lookup path (context ^. templates)
  case source of
    Just source -> do
      need [source]
    Nothing -> return ()
  return template

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
  devRun <- liftIO isDevelopmentRun
  correct <- correctSupportInstalled
  unless (correct || devRun) $ do
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
    copySupportFiles (context ^. templateSource) Copy publicDir
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

templateSourceA :: Action TemplateSource
templateSourceA = _templateSource <$> actionContext

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
