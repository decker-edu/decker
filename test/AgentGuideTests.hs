{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests the examples in the agent guide topics and the users' guide against
-- the decker executable, so they cannot silently drift from the implementation.
-- Also checks that the users' guide is complete.
--
-- An example is a fenced code block with the class @decker-example@:
--
-- > ```markdown {.decker-example file="x-deck.md" project="p" expect="code" requires="dot"}
--
-- @file@ is the path the block is written to. Blocks of one topic with the
-- same @project@ are written into the same project; a block without
-- @project@ is a project of its own. The project's decks, pages and questions
-- are built with @--strict --diagnostics-json@. Without @expect@ the build must
-- succeed without any diagnostic. With @expect="<code>"@ it must report
-- exactly that code, with @expect="error"@ it must fail with a hard error.
-- @requires@ lists programs. Without them, the example is skipped. With them, it
-- must not print an error, because a failing program does not fail a build.
module AgentGuideTests (agentGuideTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Control.Monad (filterM, forM_, unless)
import Data.List (isInfixOf, isSuffixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DeckerRun
import Network.Socket
import System.Console.GetOpt (OptDescr (..))
import System.Directory (doesDirectoryExist, findExecutable, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (StdStream (..), proc, std_err, std_out, withCreateProcess)
import System.Timeout (timeout)
import Test.Hspec
import Text.Pandoc (Block (Header), Inline (Link), def, pandocExtensions, readMarkdown, readerExtensions, runPure)
import Text.Pandoc.Walk (query)
import Text.Decker.Project.AgentDocs (replaceSection)
import Text.Decker.Project.Shake (deckerCommands, deckerFlags)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

data GuideExample = GuideExample
  { exFile :: FilePath,
    exProject :: Maybe Text,
    exExpect :: Maybe Text,
    exRequires :: [Text],
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
                then GuideExample (T.unpack (attr "file" info)) (attrMaybe "project" info) (attrMaybe "expect" info) (maybe [] T.words (attrMaybe "requires" info)) (T.unlines body) : others
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
usersGuideUrl = "https://decker-edu.github.io/decker/users-guide/decker-users-guide.html"

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

-- | The internal links of a document without a heading to lead to. It uses the
-- identifiers of Pandoc, which publishes the guide.
brokenLinks :: Text -> [Text]
brokenLinks guide =
  case runPure (readMarkdown def {readerExtensions = pandocExtensions} guide) of
    Left err -> [T.pack (show err)]
    Right doc -> nub (filter (`notElem` query headerId doc) (query linkTarget doc))
  where
    headerId (Header _ (ident, _, _) _) = [ident]
    headerId _ = []
    linkTarget (Link _ _ (url, _)) | Just target <- T.stripPrefix "#" url = [target]
    linkTarget _ = []

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

    it "has a heading for every command" $ do
      let documented command = any (=~ ("decker " <> T.pack command <> "([^a-z-]|$)" :: Text)) titles
      filter (not . documented) (sort (["agent-guide", "info", "publish"] <> deckerCommands))
        `shouldBe` []

    it "has a heading for every target" $ do
      rules <- T.readFile "app/Decker.hs"
      let targets = [target | [_, target] <- rules =~ ("phony \"([a-z-]+)\"" :: Text) :: [[Text]], target /= "pdf-start"]
      let documented target = any (=~ ("decker " <> target <> "([^a-z-]|$)" :: Text)) titles
      targets `shouldNotBe` []
      filter (not . documented) targets `shouldBe` []

    it "has a heading for every option" $ do
      let documented option = any (=~ ("--" <> T.pack option <> "([^a-z-]|$)" :: Text)) titles
      filter (not . documented) (sort (concat [long | Option _ long _ _ <- deckerFlags]))
        `shouldBe` []

    it "has no empty sections" $
      emptySections (sections guide) `shouldBe` []

    it "has no TODOs" $
      filter ("TODO" `T.isInfixOf`) (proseLines guide) `shouldBe` []

    it "has only internal links that lead to a heading" $
      brokenLinks guide `shouldBe` []

    it "is linked from the help text" $
      withProject [] $ \dir -> do
        run <- runDecker dir ["--help"]
        runStdout run `shouldContain` T.unpack usersGuideUrl

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
      forM_ ["decks", "media", "pages", "quests", "quizzes", "config", "templates"] $ \topic ->
        runStdout run `shouldContain` ("`decker agent-guide " <> topic <> "`")

  it "lists pack topics only for projects that use the pack" $ do
    withProject [] $ \dir -> do
      run <- runDecker dir ["agent-guide"]
      runStdout run `shouldNotContain` "agent-guide mono"
    withProject [("decker.yaml", "resource-pack: exe:mono\n")] $ \dir -> do
      run <- runDecker dir ["agent-guide", "mono"]
      runExit run `shouldBe` ExitSuccess
      runStdout run `shouldContain` "# Resource pack `mono`"

  it "writes an example project that builds without diagnostics" $
    withProject [] $ \dir -> do
      written <- runDecker dir ["example"]
      runExit written `shouldBe` ExitSuccess
      run <- buildTargets (dir </> "example") ["minimal-deck.md"]
      runExit run `shouldBe` ExitSuccess
      codes run `shouldBe` []

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

  describe "quizzes" $ do
    it "renders all question types in a deck" $
      withProject [("a-deck.md", "# A\n\n## {.qmc}\n\n- [X] R\n  - Tip\n- [ ] W\n\n# B\n\n## {.qmi}\n\nB1\n: I1\n\n!\n: D\n\nB2\n: !\n\n# C\n\n## {.qic}\n\n- [X] R\n\ntext\n\n- [X] S\n\n# D\n\n## {.qft}\n\n- R\n\n# E\n\n::: qft\n- [ ] W\n- [X] R\n:::\n")] $ \dir -> do
        run <- buildTargets dir ["a-deck.md"]
        runExit run `shouldBe` ExitSuccess
        html <- T.readFile (dir </> "public/a-deck.html")
        forM_ ["ul class=\"choices\"", "class=\"matchItem distractor\"", "class=\"bucket distractor\"", "<select", "class=\"quiz-ftinput\"", "class=\"quiz-tooltip\""] $ \expected ->
          html `shouldSatisfy` T.isInfixOf expected
        T.count "<select" html `shouldBe` 2
        T.count "class=\"quiz-ftinput\"" html `shouldBe` 2

    it "takes a plain list in free text as correct answers" $ do
      html <- buildMedia "# S\n\n## {.qft}\n\n- R\n- S\n"
      T.count "<li class=\"correct\">" html `shouldBe` 2

    it "applies the YAML block of a question and does not show it" $ do
      html <- buildMedia "---\nlang: en\n---\n\n# S\n\n## {.qft}\n\n``` yaml\nlang: de\nquiz:\n  style: plain\n```\n\nQ?\n\n- R\n"
      html `shouldSatisfy` T.isInfixOf "qft columns plain"
      html `shouldSatisfy` T.isInfixOf "Eingeben"
      html `shouldNotSatisfy` T.isInfixOf "<pre class=\"sourceCode"

    it "keeps other code blocks of a question" $ do
      html <- buildMedia "# S\n\n## {.qft}\n\n``` python\nkeptcode(1)\n```\n\n- R\n"
      html `shouldSatisfy` T.isInfixOf "keptcode"

    it "leaves a quiz on a page and in a handout as a plain list" $
      withProject [("a-page.md", "## {.qmc}\n\n- [X] R\n"), ("b-deck.md", "# A\n\n## {.qmc}\n\n- [X] R\n")] $ \dir -> do
        run <- buildTargets dir ["a-page.md"]
        runExit run `shouldBe` ExitSuccess
        run2 <- runDecker dir ["public/b-handout.html"]
        runExit run2 `shouldBe` ExitSuccess
        forM_ ["public/a-page.html", "public/b-handout.html"] $ \file -> do
          html <- T.readFile (dir </> file)
          html `shouldSatisfy` T.isInfixOf "task-list"
          html `shouldNotSatisfy` T.isInfixOf "ul class=\"choices\""

  describe "media" $ do
    it "chooses the element from the extension and the class" $ do
      html <-
        buildMedia
          "![](m.mp4)\n\n![](s.mp3)\n\n![](d.pdf)\n\n![](f.html)\n\n![](o.off)\n\n![](r.glb)\n\n![](g.ggb)\n\n![](d.pdf){.iframe}\n\n![](https://example.com/x)\n\n![](https://example.com/){.iframe}\n\n![](youtube:ID)\n"
      forM_
        [ "data-src=\"m.mp4\"",
          "<audio src=\"s.mp3\"",
          "type=\"application/pdf\" data=\"d.pdf\"",
          "data-src=\"f.html\"",
          "data-model=\"o.off\"",
          "data-src=\"support/mview/mview.html\"",
          "data-model=\"r.glb\"",
          "data-src=\"support/modelviewer/model-viewer.html\"",
          "data-filename=\"../.././g.ggb\"",
          "data-src=\"d.pdf\"",
          "<img src=\"https://example.com/x\"",
          "data-src=\"https://example.com\"",
          "data-src=\"https://www.youtube.com/embed/ID?"
        ]
        $ \expected -> html `shouldSatisfy` T.isInfixOf expected

    it "loads videos and iframes directly on a page" $
      withProject (("a-page.md", "![](m.mp4)\n\n![](f.html)\n\n![](youtube:ID)\n") : mediaFiles) $ \dir -> do
        run <- buildTargets dir ["a-page.md"]
        runExit run `shouldBe` ExitSuccess
        html <- T.readFile (dir </> "public/a-page.html")
        html `shouldNotSatisfy` T.isInfixOf "data-src"
        forM_ ["src=\"m.mp4\"", "src=\"f.html\"", "src=\"https://www.youtube.com/embed/ID?"] $ \expected ->
          html `shouldSatisfy` T.isInfixOf expected

    it "applies align only to images" $ do
      html <- buildMedia "![](i.svg){align=\"right\"}\n\n![](m.mp4){align=\"left\"}\n"
      html `shouldSatisfy` T.isInfixOf "<div class=\"media\" data-align=\"right\">"
      html `shouldNotSatisfy` T.isInfixOf "<div class=\"media\" data-align=\"left\">"

    it "turns the last image of a heading into the background" $ do
      html <- buildMedia "# A ![](i.svg){size=\"cover\" color=\"red\"}\n\n# B {color=\"red\"}\n\n# C ![](m.mp4){.loop}\n\n# D ![](f.html){.interactive}\n"
      forM_ ["data-background-image=\"i.svg\"", "data-background-size=\"cover\"", "data-background-color=\"red\"", "data-background-video=\"m.mp4\"", "data-background-video-loop=\"1\"", "data-background-iframe=\"f.html\"", "data-background-interactive=\"1\""] $ \expected ->
        html `shouldSatisfy` T.isInfixOf expected
      T.count "data-background-color" html `shouldBe` 1

    it "includes the marked part of a source file, or nothing" $
      withProject
        [ ("code.py", "# ---8<--- one\n  a = 1\n# --->8---\nb = 2\n\n# ---8<|--- two\nc = 3\n\nd = 4\n"),
          ("a-deck.md", "![](code.py#one){.code}\n\n![](code.py#two){.code}\n\n![](code.py#nope){.code}\n")
        ]
        $ \dir -> do
          run <- buildTargets dir ["a-deck.md"]
          runExit run `shouldBe` ExitSuccess
          html <- T.readFile (dir </> "public/a-deck.html")
          html `shouldSatisfy` T.isInfixOf "<code>a = 1\n</code>"
          html `shouldSatisfy` T.isInfixOf "<code>c = 3\n</code>"
          html `shouldSatisfy` T.isInfixOf "<code></code>"
          html `shouldNotSatisfy` T.isInfixOf "b = 2"
          html `shouldNotSatisfy` T.isInfixOf "d = 4"

    it "does not fail the build if the program of a diagram fails" $
      withProject
        [ ("decker.yaml", "external-tools:\n  dot:\n    command: decker-test-no-such-program\n"),
          ("a-deck.md", "``` {.dot .render}\ndigraph { a -> b }\n```\n")
        ]
        $ \dir -> do
          run <- buildTargets dir ["a-deck.md"]
          runExit run `shouldBe` ExitSuccess
          codes run `shouldBe` []
          runStdout run `shouldContain` "# ERROR:"

  describe "resource packs and theming" $ do
    let deck = ("a-deck.md", "# S\n") :: (FilePath, Text)
    let plugins config = withProject [("decker.yaml", config), deck] $ \dir -> do
          run <- buildTargets dir ["a-deck.md"]
          runExit run `shouldBe` ExitSuccess
          T.readFile (dir </> "public/a-deck.html")

    it "declares the palette and the CSS variables" $ do
      html <- plugins "palette:\n  contrast: 0.5\ncss-variables:\n  my-var: 7px\ncss-light-colors:\n  accent0: \"#010203\"\n"
      forM_ ["--my-var: 7px;", "--contrast: 50%;", "--accent0: #010203;"] $ \expected ->
        html `shouldSatisfy` T.isInfixOf expected

    it "replaces the palette of a pack one mode at a time" $ do
      html <- plugins "resource-pack: exe:mono\npalette:\n  colors:\n    light: [\"#111111\", \"#222222\"]\n"
      html `shouldSatisfy` T.isInfixOf "--light-shade0: #111111"
      html `shouldSatisfy` T.isInfixOf "--dark-shade0: #171f27"

    it "fails on a color that is not in the form #rrggbb" $
      withProject [("decker.yaml", "palette:\n  colors:\n    light: [red, \"#000000\"]\n"), deck] $ \dir -> do
        run <- buildTargets dir ["a-deck.md"]
        runExit run `shouldSatisfy` (/= ExitSuccess)

    it "reads the defaults and the partials of a directory pack" $
      withProject
        [ ("decker.yaml", "resource-pack: packs/my\nexclude-directories:\n  - packs\n"),
          ("packs/my/template/default.yaml", "css-variables:\n  from-pack: 1px\n"),
          ("packs/my/template/deck-title.html", "<section>MYTITLE</section>\n"),
          ("a-deck.md", "---\ntitle: T\n---\n\n# S\n")
        ]
        $ \dir -> do
          run <- buildTargets dir ["a-deck.md"]
          runExit run `shouldBe` ExitSuccess
          html <- T.readFile (dir </> "public/a-deck.html")
          html `shouldSatisfy` T.isInfixOf "--from-pack: 1px;"
          html `shouldSatisfy` T.isInfixOf "MYTITLE"

    it "fails if a pack has no default.yaml" $
      withProject [("decker.yaml", "resource-pack: mono\n"), deck] $ \dir -> do
        run <- buildTargets dir ["a-deck.md"]
        runExit run `shouldSatisfy` (/= ExitSuccess)
        (runStdout run <> runStderr run) `shouldContain` "mono/template/default.yaml"

    it "ignores resource-pack in the front matter" $
      withProject [("decker.yaml", ""), ("a-deck.md", "---\nresource-pack: exe:mono\n---\n\n# S\n")] $ \dir -> do
        run <- buildTargets dir ["a-deck.md"]
        runExit run `shouldBe` ExitSuccess
        html <- T.readFile (dir </> "public/a-deck.html")
        html `shouldNotSatisfy` T.isInfixOf "#171f27"

    it "switches plugins with mono, but not with packs that have own templates" $ do
      mono <- plugins "resource-pack: exe:mono\n"
      mono `shouldNotSatisfy` T.isInfixOf "import quizWuePlugin"
      monoQuiz <- plugins "resource-pack: exe:mono\ndecker-plugins:\n  quiz-wue: true\n"
      monoQuiz `shouldSatisfy` T.isInfixOf "import quizWuePlugin"
      tudo <- plugins "resource-pack: exe:tudo\ndecker-plugins:\n  quiz-wue: false\n"
      tudo `shouldSatisfy` T.isInfixOf "quizWuePlugin"

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
checkProject exs = do
  let required = nub (concatMap exRequires exs)
  missing <- filterM (fmap isNothing . findExecutable . T.unpack) required
  if not (null missing)
    then pendingWith ("needs " <> T.unpack (T.unwords missing))
    else withProject [(exFile e, exBody e) | e <- exs] $ \dir -> do
      run <- buildTargets dir (map exFile exs)
      let report = "\nstdout:\n" <> runStdout run <> "\nstderr:\n" <> runStderr run
      unless (null required || not ("# ERROR:" `isInfixOf` runStdout run)) $
        expectationFailure ("a program failed" <> report)
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

-- | Placeholders for the media in the tests. A build only needs them to exist.
mediaFiles :: [(FilePath, Text)]
mediaFiles =
  [ ("i.svg", "<svg xmlns=\"http://www.w3.org/2000/svg\"/>\n"),
    ("m.mp4", ""),
    ("s.mp3", ""),
    ("d.pdf", ""),
    ("f.html", ""),
    ("o.off", ""),
    ("r.glb", ""),
    ("g.ggb", "")
  ]

-- | Builds a deck with the given body and the placeholders, and returns its HTML.
buildMedia :: Text -> IO Text
buildMedia source =
  withProject (("a-deck.md", source) : mediaFiles) $ \dir -> do
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
