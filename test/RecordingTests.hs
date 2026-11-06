{-# LANGUAGE OverloadedStrings #-}

module RecordingTests (recordingTests) where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import DeckerRun (withProject)
import System.Directory
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import Text.Decker.Server.Recording

recordingTests :: Spec
recordingTests =
  describe "Recording sessions" $ do
    it "decides about a chunk by its byte offset" $ do
      decideChunk 0 0 5 `shouldBe` AppendChunk
      decideChunk 10 10 5 `shouldBe` AppendChunk
      decideChunk 10 5 5 `shouldBe` AlreadyStored
      decideChunk 10 0 10 `shouldBe` AlreadyStored
      decideChunk 10 8 5 `shouldBe` Conflict
      decideChunk 10 12 5 `shouldBe` Conflict
      decideChunk 10 (-1) 1 `shouldBe` Conflict

    it "stores duplicated and out-of-order chunks byte for byte once" $
      withSession [] AppendRecording $ \base -> do
        let chunk offset bytes = appendChunk base offset bytes
        chunk 0 "abc" `shouldReturn` Stored 3
        chunk 0 "abc" `shouldReturn` Stored 3
        chunk 6 "ghi" `shouldReturn` Mismatch 3
        chunk 3 "def" `shouldReturn` Stored 6
        chunk 5 "fgh" `shouldReturn` Mismatch 6
        chunk 3 "def" `shouldReturn` Stored 6
        chunk 6 "ghi" `shouldReturn` Stored 9
        LBS.readFile (videoPart (sessionFiles base)) `shouldReturn` "abcdefghi"

    it "refuses chunks and snapshots without a session" $
      withProject deck $ \dir -> do
        let base = dir </> "a"
        appendChunk base 0 "abc" `shouldReturn` NoSession
        putSnapshot base (timesPart (sessionFiles base)) "[]" `shouldReturn` False
        discardSession base `shouldReturn` False

    it "does not start a second session, and reports the first" $
      withSession [] AppendRecording $ \base -> do
        _ <- appendChunk base 0 "abcd"
        _ <- putSnapshot base (timesPart (sessionFiles base)) times
        result <- startSession base ReplaceRecording 0
        case result of
          Left info -> do
            siMode info `shouldBe` AppendRecording
            siSize info `shouldBe` 4
            -- The snapshot ends at 12.5 s, the session started at 2 s.
            siDuration info `shouldBe` Just 10.5
          Right () -> expectationFailure "started a second session"

    it "treats an unreadable sidecar as an append session" $
      withSession [("a-recording.webm", "one")] ReplaceRecording $ \base -> do
        writeFile (sidecarFile (sessionFiles base)) "garbage"
        _ <- appendChunk base 0 "two"
        fmap siMode <$> sessionInfo base `shouldReturn` Just AppendRecording
        finishSession base `shouldReturn` Just (base <> "-recording-1.webm")

    it "replaces the recording and the timings on finish" $
      withSession old ReplaceRecording $ \base -> do
        _ <- appendChunk base 0 "new"
        _ <- putSnapshot base (timesPart (sessionFiles base)) times
        finishSession base `shouldReturn` Just (base <> "-recording.webm")
        files <- recordingFiles base
        files `shouldBe` ["a-deck.md", "a-recording.webm", "a-times.json", "decker.yaml"]
        LBS.readFile (base <> "-recording.webm") `shouldReturn` "new"
        LBS.readFile (base <> "-times.json") `shouldReturn` times
        -- Default permissions, not those of a durable temp file.
        forM_ ["-recording.webm", "-times.json"] $ \suffix -> do
          permissions <- getPermissions (base <> suffix)
          (readable permissions, writable permissions) `shouldBe` (True, True)

    it "appends numbered fragments on finish" $
      withSession [("a-recording.webm", "one")] AppendRecording $ \base -> do
        _ <- appendChunk base 0 "two"
        finishSession base `shouldReturn` Just (base <> "-recording-1.webm")
        startSession base AppendRecording 0 `shouldReturn` Right ()
        _ <- appendChunk base 0 "three"
        finishSession base `shouldReturn` Just (base <> "-recording-2.webm")
        files <- recordingFiles base
        files `shouldBe` ["a-deck.md", "a-recording-0.webm", "a-recording-1.webm", "a-recording-2.webm", "decker.yaml"]
        mapM (LBS.readFile . (takeDirectory base </>)) ["a-recording-0.webm", "a-recording-1.webm", "a-recording-2.webm"]
          `shouldReturn` ["one", "two", "three"]

    it "keeps the old recording if the session is empty or discarded" $ do
      withSession old ReplaceRecording $ \base -> do
        finishSession base `shouldReturn` Nothing
        recordingFiles base `shouldReturn` oldFiles
      withSession old ReplaceRecording $ \base -> do
        _ <- appendChunk base 0 "new"
        _ <- putSnapshot base (timesPart (sessionFiles base)) times
        discardSession base `shouldReturn` True
        recordingFiles base `shouldReturn` oldFiles
        LBS.readFile (base <> "-times.json") `shouldReturn` "old"

    it "accepts only deck paths" $ do
      validDeckBase "test/decks/ar" `shouldReturn` Just "test/decks/ar"
      mapM validDeckBase ["", "/etc/passwd", "../decker/test/decks/ar", "public/ar", "test/decks/nope"]
        `shouldReturn` replicate 5 Nothing
      validDeckBase "./test/decks/ar" `shouldReturn` Just "test/decks/ar"
  where
    deck = [("a-deck.md", "# Slide\n")]
    old = [("a-recording-0.webm", "old0"), ("a-recording-1.webm", "old1"), ("a-recording.mp4", "mp4"), ("a-times.json", "old")]
    oldFiles = sort $ ["a-deck.md", "decker.yaml"] <> map fst old
    times = "[{\"slideId\":\"s1\",\"timeIn\":\"2\",\"timeOut\":\"12.5\"}]"

-- | Runs the action on a fresh project with the deck @a-deck.md@, the given
-- files and a running session, and passes the deck base name.
withSession :: [(FilePath, Text)] -> RecordingMode -> (FilePath -> IO a) -> IO a
withSession files mode action =
  withProject ([("a-deck.md", "# Slide\n")] <> files) $ \dir -> do
    let base = dir </> "a"
    startSession base mode 2 `shouldReturn` Right ()
    action base

-- | The files next to the deck, without the parts of a session. Fails if
-- there are parts left.
recordingFiles :: FilePath -> IO [FilePath]
recordingFiles base = do
  files <- sort <$> listDirectory (takeDirectory base)
  filter (\f -> any (`isSuffixOf` f) [".part", ".part.json"]) files `shouldBe` []
  return $ filter (/= "public") files
