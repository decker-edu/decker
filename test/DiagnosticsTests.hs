{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsTests (diagnosticsTests) where

import Control.Monad (forM_)
import Data.Aeson (Value (..), decode, eitherDecodeFileStrict)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import DeckerRun
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import Text.Decker.Internal.Diagnostics (Diagnostic (..))

fixtures :: FilePath
fixtures = "test/fixtures/diagnostics"

diagnosticsTests :: Spec
diagnosticsTests = describe "Diagnostics (runs the decker executable)" $ do
  names <- runIO $ listDirectory fixtures
  -- Each fixture directory is named after the one diagnostic code it
  -- produces.
  forM_ names $ \code ->
    it ("reports " <> code) $ do
      files <- copyProject (fixtures </> code)
      withProject files $ \dir -> do
        run <- buildTargets dir (map fst files)
        codes run `shouldBe` [T.pack code]

  it "fails a strict build on warnings, not on hints" $ do
    warn <- copyProject (fixtures </> "citation-without-bibliography")
    withProject warn $ \dir -> do
      lenient <- runDecker dir (targetsOf (map fst warn))
      runExit lenient `shouldBe` ExitSuccess
      strict <- runDecker dir ("--strict" : targetsOf (map fst warn))
      runExit strict `shouldBe` ExitFailure 1
    hint <- copyProject (fixtures </> "setext-heading")
    withProject hint $ \dir -> do
      strict <- buildTargets dir (map fst hint)
      runExit strict `shouldBe` ExitSuccess

  it "honours strict: true in decker.yaml" $ do
    files <- copyProject (fixtures </> "citation-without-bibliography")
    withProject (("decker.yaml", "strict: true\n") : files) $ \dir -> do
      run <- runDecker dir (targetsOf (map fst files))
      runExit run `shouldBe` ExitFailure 1

  it "reports the diagnostics of up-to-date targets again" $ do
    files <- copyProject (fixtures </> "citation-without-bibliography")
    withProject files $ \dir -> do
      _ <- buildTargets dir (map fst files)
      again <- buildTargets dir (map fst files)
      codes again `shouldBe` ["citation-without-bibliography"]
      runExit again `shouldBe` ExitFailure 1

  it "names the included file in parser hints" $ do
    files <- copyProject (fixtures </> "setext-heading")
    withProject files $ \dir -> do
      run <- buildTargets dir (map fst files)
      map diagSource (runDiagnostics run) `shouldBe` ["part.md"]
      map diagLine (runDiagnostics run) `shouldBe` [Just 3]
      map diagTarget (runDiagnostics run) `shouldBe` [Just "public/main-deck.html"]

  it "rebuilds the exam catalog when a question file is added" $ do
    files <- copyProject (fixtures </> "quest-unknown-field")
    withProject files $ \dir -> do
      let catalog = "public/exam-builder/questions.json"
      _ <- runDecker dir [catalog]
      before <- T.readFile (dir </> catalog)
      ("Second question" `T.isInfixOf` before) `shouldBe` False
      writeFile (dir </> "second-quest.yaml") (secondQuestion "Second question")
      run <- runDecker dir [catalog]
      runExit run `shouldBe` ExitSuccess
      after <- T.readFile (dir </> catalog)
      ("Second question" `T.isInfixOf` after) `shouldBe` True

  it "keeps stdout clean for info --json" $ do
    files <- copyProject "test/fixtures/info/project"
    withProject files $ \dir -> do
      run <- runDecker dir ["info", "--json"]
      runExit run `shouldBe` ExitSuccess
      (decode (LBS.pack (runStdout run)) :: Maybe Value) `shouldSatisfy` (/= Nothing)

  it "prints the project facts of info --json (golden)" $ do
    files <- copyProject "test/fixtures/info/project"
    withProject files $ \dir -> do
      run <- runDecker dir ["info", "--json"]
      expected <- either fail return =<< eitherDecodeFileStrict "test/fixtures/info/expected.json"
      fmap normalise (decode (LBS.pack (runStdout run))) `shouldBe` Just (expected :: Value)
      ("do not leak" `isInfixOf` runStdout run) `shouldBe` False
  where
    secondQuestion title =
      unlines
        [ "LectureId: l",
          "TopicId: t2",
          "Title: " <> title,
          "Points: 1",
          "Difficulty: Easy",
          "Comment: \"\"",
          "Question: Which?",
          "Answer:",
          "  tag: MultipleChoice",
          "  Choices:",
          "    - Correct: true",
          "      TheAnswer: \"yes\""
        ]

-- | Replaces the values that depend on the machine and the Decker build.
normalise :: Value -> Value
normalise (Object o) =
  Object $
    KeyMap.insert "decker" (String "<decker>") $
      adjust (setKey "project" "<project>") "directories" $
        adjust (setKey "resolved" "<resolved>") "resourcePack" o
  where
    setKey key value (Object inner) = Object (adjust (const (String value)) key inner)
    setKey _ _ other = other
    adjust f key m = maybe m (\v -> KeyMap.insert key (f v) m) (KeyMap.lookup key m)
normalise other = other
