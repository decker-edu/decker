{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests the examples in the agent guide topics against the decker
-- executable, so the guide cannot silently drift from the implementation.
--
-- An example is a fenced code block with the class @decker-example@:
--
-- > ```markdown {.decker-example file="x-deck.md" project="p" expect="code"}
--
-- @file@ is the path the block is written to. Blocks of one topic with the
-- same @project@ are written into the same project; a block without
-- @project@ is a project of its own. The project's decks, pages and questions
-- are built with @--strict --diagnostics-json@. Without @expect@ the build must
-- succeed without any diagnostic. With @expect="<code>"@ it must report
-- exactly that code, with @expect="error"@ it must fail with a hard error.
module AgentGuideTests (agentGuideTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf, isSuffixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DeckerRun
import Network.Socket
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (StdStream (..), proc, std_err, std_out, withCreateProcess)
import System.Timeout (timeout)
import Test.Hspec
import Text.Decker.Project.AgentDocs (replaceSection)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

data GuideExample = GuideExample
  { exFile :: FilePath,
    exProject :: Maybe Text,
    exExpect :: Maybe Text,
    exBody :: Text
  }
  deriving (Eq, Show)

-- | Extracts the examples from a topic file. Fenced blocks are skipped as a
-- whole, so fences inside an example or another block are not misread.
examples :: Text -> [GuideExample]
examples = go . T.lines
  where
    go [] = []
    go (line : rest) =
      case opening line of
        Just (fence, info) ->
          let (body, after) = break (closes fence) rest
              others = go (drop 1 after)
           in if "decker-example" `elem` classes info
                then GuideExample (T.unpack (attr "file" info)) (attrMaybe "project" info) (attrMaybe "expect" info) (T.unlines body) : others
                else others
        Nothing -> go rest
    opening line =
      let stripped = T.stripStart line
          run c = T.takeWhile (== c) stripped
       in listToMaybe
            [ (fence, T.drop (T.length fence) stripped)
              | c <- ['`', '~'],
                let fence = run c,
                T.length fence >= 3
            ]
    closes fence line =
      let stripped = T.strip line
       in T.length stripped >= T.length fence && T.all (== T.head fence) stripped
    classes info = [T.drop 1 w | w <- T.words (T.filter (`notElem` ['{', '}']) info), "." `T.isPrefixOf` w]
    attrMaybe key info =
      case info =~ (key <> "=\"([^\"]*)\"" :: Text) :: [[Text]] of
        [[_, value]] -> Just value
        _ -> Nothing
    attr key info = fromMaybe (error $ "example without " <> T.unpack key <> ": " <> T.unpack info) (attrMaybe key info)

-- | All topic files of Decker and of the bundled resource packs.
topicFiles :: IO [FilePath]
topicFiles = do
  packs <- listDirectory "resource"
  concat <$> mapM topics packs
  where
    topics pack = do
      let dir = "resource" </> pack </> "template" </> "agent-guide"
      exists <- doesDirectoryExist dir
      if exists
        then map (dir </>) . filter (".md" `isSuffixOf`) <$> listDirectory dir
        else return []

-- | Groups the examples of one topic into projects.
projects :: [GuideExample] -> [(Text, [GuideExample])]
projects exs =
  Map.toList (Map.fromListWith (flip (<>)) [(p, [e]) | e <- exs, Just p <- [exProject e]])
    <> [(T.pack (exFile e), [e]) | e <- exs, exProject e == Nothing]

agentGuideTests :: Spec
agentGuideTests = describe "Agent guide" $ do
  files <- runIO topicFiles
  forM_ files $ \topic -> do
    content <- runIO $ T.readFile topic
    describe topic $
      forM_ (projects (examples content)) $ \(name, exs) ->
        it ("example " <> T.unpack name) $ checkProject exs

  it "documents a fixture for every diagnostic code in the index" $ do
    index <- T.readFile "resource/decker/template/agent-guide/index.md"
    fixtures <- listDirectory "test/fixtures/diagnostics"
    let documented = [code | line <- T.lines index, [_, code] <- line =~ ("^\\| `([a-z-]+)` +\\|" :: Text) :: [[Text]]]
    documented `shouldNotBe` []
    forM_ documented $ \code -> map T.pack fixtures `shouldContain` [code]

  it "lists the topics in the index" $
    withProject [] $ \dir -> do
      run <- runDecker dir ["agent-guide"]
      runExit run `shouldBe` ExitSuccess
      forM_ ["decks", "pages", "quests", "config", "templates"] $ \topic ->
        runStdout run `shouldContain` ("`decker agent-guide " <> topic <> "`")

  it "lists pack topics only for projects that use the pack" $ do
    withProject [] $ \dir -> do
      run <- runDecker dir ["agent-guide"]
      runStdout run `shouldNotContain` "agent-guide mono"
    withProject [("decker.yaml", "resource-pack: exe:mono\n")] $ \dir -> do
      run <- runDecker dir ["agent-guide", "mono"]
      runExit run `shouldBe` ExitSuccess
      runStdout run `shouldContain` "# Resource pack `mono`"

  it "exits decker serve with status 7 if the port is in use" $
    withProject [] $ \dir -> do
      exe <- deckerExecutable
      port <- freePort
      let serve = proc exe ["--project-dir=" <> dir, "serve", "-p", show port]
      -- Pipes, not closed handles: the server writes to stdout.
      withCreateProcess serve {std_out = CreatePipe, std_err = CreatePipe} $ \_ _ _ _ -> do
        waitForServer port
        second <- timeout 30000000 (runDecker dir ["serve", "-p", show port])
        fmap runExit second `shouldBe` Just (ExitFailure 7)

  it "fails on an unknown topic" $
    withProject [] $ \dir -> do
      run <- runDecker dir ["agent-guide", "no-such-topic"]
      runExit run `shouldBe` ExitFailure 1

  describe "AGENTS.md section" $ do
    it "is appended to an existing file" $
      replaceSection "body" "# Own\n" `shouldBe` "# Own\n\n<!-- decker:begin -->\nbody\n<!-- decker:end -->\n"
    it "replaces only the delimited section" $
      replaceSection "new" "a\n<!-- decker:begin -->\nold\n<!-- decker:end -->\nb\n"
        `shouldBe` "a\n<!-- decker:begin -->\nnew\n<!-- decker:end -->\nb\n"
    it "is idempotent" $ do
      let once = replaceSection "body" "# Own\n"
      replaceSection "body" once `shouldBe` once

checkProject :: [GuideExample] -> Expectation
checkProject exs =
  withProject [(exFile e, exBody e) | e <- exs] $ \dir -> do
    run <- buildTargets dir (map exFile exs)
    let report = "\nstdout:\n" <> runStdout run <> "\nstderr:\n" <> runStderr run
    case nub (mapMaybe exExpect exs) of
      [] -> do
        unless (null (codes run)) $ expectationFailure ("unexpected diagnostics " <> show (codes run) <> report)
        unless (runExit run == ExitSuccess) $ expectationFailure ("build failed" <> report)
      ["error"] ->
        unless ("Error when running Shake" `isInfixOf` (runStdout run <> runStderr run)) $
          expectationFailure ("expected a hard error" <> report)
      expected ->
        unless (codes run == sort expected) $
          expectationFailure ("expected " <> show expected <> ", got " <> show (codes run) <> report)

-- | A port that is free right now.
freePort :: IO Int
freePort =
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    fromIntegral <$> socketPort sock

-- | Waits up to 30 seconds until something accepts connections on the port.
waitForServer :: Int -> IO ()
waitForServer port = go (300 :: Int)
  where
    go 0 = expectationFailure "decker serve did not start"
    go n = do
      up <- try (bracket (socket AF_INET Stream defaultProtocol) close connectTo)
      case up of
        Right () -> return ()
        Left (_ :: IOException) -> threadDelay 100000 >> go (n - 1)
    connectTo sock = connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
