{-# LANGUAGE OverloadedStrings #-}

module CompanionTests (companionTests) where

import Data.Text.IO qualified as T
import DeckerRun
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import Text.Decker.Internal.Companions

companionTests :: Spec
companionTests = do
  describe "Companions" $ do
    it "recognizes published companions" $ do
      map
        isPublishedCompanion
        [ "a-annot.json",
          "dir/a-manip.json",
          "a-times.json",
          "a-transcript.json",
          "a-recording.mp4",
          "a-recording.vtt",
          "a-recording-de.vtt"
        ]
        `shouldBe` replicate 7 True
      map
        isPublishedCompanion
        ["a-poll.json", "-annot.json", "a-recording-.vtt", "a-deck.md", "a-recording.webm"]
        `shouldBe` replicate 5 False

    it "uploads JSON and VTT companions and polls, but not videos" $ do
      map isUploadable ["a-annot.json", "a-recording.vtt", "a-poll.json"]
        `shouldBe` [True, True, True]
      map isUploadable ["a-recording.mp4", "a-recording-de.vtt", "a-deck.md"]
        `shouldBe` [False, False, False]

    it "recognizes recording artifacts" $ do
      map
        isRecordingArtifact
        [ "a-recording.webm",
          "a-recording-3.webm",
          "a-recording.mp4",
          "a-recording.webm.part",
          "a-recording.part.json"
        ]
        `shouldBe` replicate 5 True
      map isRecordingArtifact ["a-annot.json", "a.webm", "a-recording.vtt"]
        `shouldBe` [False, False, False]

    it "matches companions to their deck" $ do
      deckBase "dir/a-deck.md" `shouldBe` Just "dir/a"
      deckBase "dir/-deck.md" `shouldBe` Nothing
      "dir/a-deck.md" `isCompanionOf` "dir/a-annot.json" `shouldBe` True
      "dir/a-deck.md" `isCompanionOf` "dir/a-recording-en.vtt" `shouldBe` True
      "dir/a-deck.md" `isCompanionOf` "a-annot.json" `shouldBe` False
      "dir/a-deck.md" `isCompanionOf` "dir/ba-annot.json" `shouldBe` False
      "dir/a-deck.md" `isCompanionOf` "dir/a-poll.json" `shouldBe` False
      "a-deck.md" `isCompanionOf` "a-annot.json" `shouldBe` True

  describe "Companion files (runs the decker executable)" $
    it "are copied and pruned without rebuilding the deck" $
      withProject [("a-deck.md", "# Slide\n\nText\n")] $ \dir -> do
        let deck = dir </> "public/a-deck.html"
            public file = dir </> "public" </> file
            build = do
              run <- runDecker dir ["decks"]
              runExit run `shouldBe` ExitSuccess
        build
        built <- getModificationTime deck

        -- Added: copied, deck not rebuilt.
        T.writeFile (dir </> "a-annot.json") "{\"version\":1}"
        T.writeFile (dir </> "a-poll.json") "{}"
        build
        T.readFile (public "a-annot.json") `shouldReturn` "{\"version\":1}"
        doesFileExist (public "a-poll.json") `shouldReturn` False
        getModificationTime deck `shouldReturn` built

        -- Changed: the copy follows.
        T.writeFile (dir </> "a-annot.json") "{\"version\":2}"
        build
        T.readFile (public "a-annot.json") `shouldReturn` "{\"version\":2}"
        getModificationTime deck `shouldReturn` built

        -- Captions matched by pattern are picked up on the next build.
        T.writeFile (dir </> "a-recording-de.vtt") "WEBVTT\n"
        build
        doesFileExist (public "a-recording-de.vtt") `shouldReturn` True

        -- Deleted: the copy is pruned.
        removeFile (dir </> "a-annot.json")
        build
        doesFileExist (public "a-annot.json") `shouldReturn` False
        getModificationTime deck `shouldReturn` built
