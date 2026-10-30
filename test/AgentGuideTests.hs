{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests the examples in the agent guide topics and the users' guide against
-- the decker executable, so they cannot silently drift from the implementation.
-- Also tracks what the users' guide still lacks, see 'undocumentedCommands' and
-- the numbers below it.
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
import System.Console.GetOpt (OptDescr (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (StdStream (..), proc, std_err, std_out, withCreateProcess)
import System.Timeout (timeout)
import Test.Hspec
import Text.Decker.Project.AgentDocs (replaceSection)
import Text.Decker.Project.Shake (deckerCommands, deckerFlags)
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
    classes info = [T.drop 1 w | w <- T.words (T.filter (`notElem` ['{', '}']) info), "." `T.isPrefixOf` w]
    attrMaybe key info =
      case info =~ (key <> "=\"([^\"]*)\"" :: Text) :: [[Text]] of
        [[_, value]] -> Just value
        _ -> Nothing
    attr key info = fromMaybe (error $ "example without " <> T.unpack key <> ": " <> T.unpack info) (attrMaybe key info)

-- | The fence and the info string of a line that opens a fenced block.
opening :: Text -> Maybe (Text, Text)
opening line =
  let stripped = T.stripStart line
      run c = T.takeWhile (== c) stripped
   in listToMaybe
        [ (fence, T.drop (T.length fence) stripped)
          | c <- ['`', '~'],
            let fence = run c,
            T.length fence >= 3
        ]

closes :: Text -> Text -> Bool
closes fence line =
  let stripped = T.strip line
   in T.length stripped >= T.length fence && T.all (== T.head fence) stripped

usersGuide :: FilePath
usersGuide = "users-guide/decker-users-guide.md"

usersGuideUrl :: Text
usersGuideUrl = "https://github.com/decker-edu/decker/blob/develop/users-guide/decker-users-guide.md"

-- | Commands that have no heading in the users' guide yet. Remove a command
-- when its section is written.
undocumentedCommands :: [String]
undocumentedCommands = []

-- | Long options that have no heading in the users' guide yet.
undocumentedOptions :: [String]
undocumentedOptions = []

-- | Sections of the users' guide without any content. Lower this number when a
-- section is written. See doc/users-guide-plan.md.
emptySectionCount :: Int
emptySectionCount = 20

-- | @TODO@ placeholders in the users' guide. Lower this number when one is
-- filled in.
todoCount :: Int
todoCount = 0

-- | Keys in the @default.yaml@ of the base pack that do not need a place in the
-- reference: test values, and @progress@, which nothing reads.
unreferencedDefaults :: [Text]
unreferencedDefaults = ["progress", "test"]

-- | The lines of a document with every fenced block reduced to its fence, so
-- that code is neither read as a heading nor mistaken for an empty section.
proseLines :: Text -> [Text]
proseLines = go . T.lines
  where
    go [] = []
    go (line : rest) =
      case opening line of
        Just (fence, _) -> fence : go (drop 1 (dropWhile (not . closes fence) rest))
        Nothing -> line : go rest

data Section = Section
  { secLevel :: Int,
    secTitle :: Text,
    secHasBody :: Bool
  }

sections :: Text -> [Section]
sections = go . proseLines
  where
    go [] = []
    go (line : rest)
      | isHeading line =
          let (body, after) = break isHeading rest
           in Section (T.length (T.takeWhile (== '#') line)) (title line) (any (not . T.null . T.strip) body) : go after
      | otherwise = go rest
    isHeading line = line =~ ("^#{1,6} " :: Text)
    title = T.strip . T.takeWhile (/= '{') . T.dropWhile (== '#')

-- | The titles of the sections that have neither a body nor subsections.
emptySections :: [Section] -> [Text]
emptySections secs =
  [ secTitle sec
    | (sec, next) <- zip secs (map Just (drop 1 secs) <> [Nothing]),
      not (secHasBody sec),
      maybe True (\n -> secLevel n <= secLevel sec) next
  ]

-- | The sections of the users' guide that an agent topic refers to, as in
-- "See the [users' guide](url), sections *One* and *Two*."
guideReferences :: Text -> [Text]
guideReferences content =
  [ section
    | [_, names] <- T.unwords (T.words content) =~ ("users' guide\\]\\([^)]*\\)[^.]*sections? ([^.]*)\\." :: Text) :: [[Text]],
      [_, section] <- names =~ ("\\*([^*]+)\\*" :: Text) :: [[Text]]
  ]

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
  topics <- runIO topicFiles
  forM_ (usersGuide : topics) $ \topic -> do
    content <- runIO $ T.readFile topic
    describe topic $
      forM_ (projects (examples content)) $ \(name, exs) ->
        it ("example " <> T.unpack name) $ checkProject exs

  describe "users' guide" $ do
    guide <- runIO $ T.readFile usersGuide
    let titles = map secTitle (sections guide)

    it "has a heading for every command, except the known gaps" $ do
      let documented command = any (=~ ("decker " <> T.pack command <> "([^a-z-]|$)" :: Text)) titles
      filter (not . documented) (sort (["agent-guide", "info", "publish"] <> deckerCommands))
        `shouldBe` undocumentedCommands

    it "has a heading for every target" $ do
      rules <- T.readFile "app/Decker.hs"
      let targets = [target | [_, target] <- rules =~ ("phony \"([a-z-]+)\"" :: Text) :: [[Text]], target /= "pdf-start"]
      let documented target = any (=~ ("decker " <> target <> "([^a-z-]|$)" :: Text)) titles
      targets `shouldNotBe` []
      filter (not . documented) targets `shouldBe` []

    it "has a heading for every option, except the known gaps" $ do
      let documented option = any (=~ ("--" <> T.pack option <> "([^a-z-]|$)" :: Text)) titles
      filter (not . documented) (sort (concat [long | Option _ long _ _ <- deckerFlags]))
        `shouldBe` undocumentedOptions

    it "has the known number of empty sections" $
      length (emptySections (sections guide)) `shouldBe` emptySectionCount

    it "has the known number of TODOs" $
      length (filter ("TODO" `T.isInfixOf`) (proseLines guide)) `shouldBe` todoCount

    it "explains every key of the base pack's defaults" $ do
      defaults <- T.readFile "resource/decker/template/default.yaml"
      let keys = [key | line <- T.lines defaults, [_, key] <- line =~ ("^([a-z][a-z-]*):" :: Text) :: [[Text]]]
      keys `shouldNotBe` []
      filter (\key -> not (("`" <> key) `T.isInfixOf` guide)) (filter (`notElem` unreferencedDefaults) keys) `shouldBe` []

    forM_ topics $ \topic ->
      it ("is linked correctly from " <> topic) $ do
        content <- T.readFile topic
        let mentions = T.count "users' guide" content
        T.count ("[users' guide](" <> usersGuideUrl <> ")") content `shouldBe` mentions
        length (guideReferences content) `shouldSatisfy` (>= mentions)
        forM_ (guideReferences content) $ \section -> titles `shouldContain` [section]

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

  it "loads template.js-module on pages" $
    withProject
      [ ("decker.yaml", "template:\n  js:\n    - classic.js\n  js-module:\n    - module.js\n"),
        ("classic.js", ""),
        ("module.js", ""),
        ("a-page.md", "---\ntitle: A\n---\n\nText\n")
      ]
      $ \dir -> do
        run <- buildTargets dir ["a-page.md"]
        runExit run `shouldBe` ExitSuccess
        page <- T.readFile (dir </> "public/a-page.html")
        page `shouldSatisfy` T.isInfixOf "<script type=\"module\" src=\"module.js\">"
        T.count "src=\"classic.js\"" page `shouldBe` 1

  describe "template macros" $ do
    it "expands positional parameters, defaults and the colon form" $ do
      html <-
        buildDeck
          "templates:\n  m: \":(1)-:(2)-:(3)-:(lang|en)\"\n"
          "---\ntitle: T\n---\n\n# S\n\n[@m a b](u)\n\n[m: c d](u)\n"
      html `shouldSatisfy` T.isInfixOf "a-b-3-en"
      html `shouldSatisfy` T.isInfixOf "c-d-3-en"

    it "does not insert the document for a code block macro in the front matter" $ do
      html <-
        buildDeck
          ""
          "---\ntitle: T\ntemplates:\n  m: \"X :(1) :(code)\"\n---\n\n# First\n\nMARKER\n\n# Second\n\n``` {macro=\"m\" .a}\nthe code\n```\n"
      T.count "MARKER" html `shouldBe` 1
      html `shouldSatisfy` T.isInfixOf "X a the code"

  describe "slide layout" $ do
    it "keeps a level-3 heading of three words" $ do
      html <- buildDeck "" "# S\n\n### Foo bar baz\n\nA\n\nB\n\nC\n"
      html `shouldSatisfy` T.isInfixOf "Foo bar baz"

    it "makes columns from a heading of numbers" $ do
      html <- buildDeck "" "# S\n\n### 1 1 2\n\nA\n\nB\n\nC\n"
      html `shouldSatisfy` T.isInfixOf "width:50%;float:left"

    it "removes content in front of the first area" $ do
      html <- buildDeck "" "# S {.columns}\n\nGONE\n\n## L {.left}\n\nKEPT\n"
      html `shouldSatisfy` T.isInfixOf "KEPT"
      html `shouldNotSatisfy` T.isInfixOf "GONE"

    it "removes code blocks with the class comment" $ do
      html <- buildDeck "" "# S\n\nKEPT\n\n``` comment\nGONE\n```\n"
      html `shouldSatisfy` T.isInfixOf "KEPT"
      html `shouldNotSatisfy` T.isInfixOf "GONE"

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

-- | Builds a deck in a project with the given @decker.yaml@ and returns its HTML.
buildDeck :: Text -> Text -> IO Text
buildDeck config source =
  withProject [("decker.yaml", config), ("a-deck.md", source)] $ \dir -> do
    run <- buildTargets dir ["a-deck.md"]
    runExit run `shouldBe` ExitSuccess
    T.readFile (dir </> "public/a-deck.html")

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
