{-# LANGUAGE OverloadedStrings #-}

module ServerTests (serverTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, forM)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (isSuffixOf)
import Data.Text.IO qualified as T
import DeckerRun
import Network.HTTP.Client
import Network.HTTP.Types (Header, statusCode)
import Network.Socket.Free (getFreePort)
import System.Directory
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import System.Process
import Test.Hspec
import Text.Decker.Internal.Common (mkTmpDirName)

serverTests :: Spec
serverTests =
  describe "Dev server uploads (runs the decker executable)" $
    it "stores companions in the project dir without a build" $
      withProject [("a-deck.md", "# Slide\n\nText\n"), ("a-annot.json", "{\"v\":1}")] $ \dir ->
        withServer dir $ \requestWith -> do
          let request = requestWith []
          let public file = dir </> "public" </> file
              status method path body = statusCode . responseStatus <$> request method path body
              get path = responseBody <$> request "GET" path ""
          -- The initial build copies the companion.
          T.readFile (public "a-annot.json") `shouldReturn` "{\"v\":1}"

          status "PUT" "a-annot.json" "{\"v\":2}" `shouldReturn` 200
          T.readFile (dir </> "a-annot.json") `shouldReturn` "{\"v\":2}"
          -- Served from the project dir, not from the stale public copy.
          get "a-annot.json" `shouldReturn` "{\"v\":2}"

          status "OPTIONS" "a-poll.json" "" `shouldReturn` 204
          -- The whiteboard enables autosave only for a server with this marker.
          probe <- request "OPTIONS" "a-annot.json" ""
          lookup "X-Decker-Server" (responseHeaders probe) `shouldBe` Just "1"
          status "PUT" "a-poll.json" "{}" `shouldReturn` 200
          status "OPTIONS" "a-poll.json" "" `shouldReturn` 200

          -- Never into the build output, never anything but companions.
          status "PUT" "public/a-annot.json" "{}" `shouldReturn` 406
          status "PUT" "a-deck.md" "" `shouldReturn` 406

          -- No build ran: it would have updated the public copy.
          threadDelay 2000000
          T.readFile (public "a-annot.json") `shouldReturn` "{\"v\":1}"
          parts <- filter (".part" `isSuffixOf`) <$> listDirectory dir
          parts `shouldBe` []

          -- The first overwrite saved the old version, later ones do not.
          status "PUT" "a-annot.json" "{\"v\":3}" `shouldReturn` 200
          backups <- backupsOf dir
          map snd backups `shouldBe` ["{\"v\":1}"]

          -- Recordings come from the project dir too, with ranges for seeking.
          LBS.writeFile (dir </> "a-recording.mp4") "0123456789"
          status "HEAD" "a-recording.mp4" "" `shouldReturn` 200
          ranged <- requestWith [("Range", "bytes=2-5")] "GET" "a-recording.mp4" ""
          statusCode (responseStatus ranged) `shouldBe` 206
          responseBody ranged `shouldBe` "2345"

          -- A companion deleted from the project dir is gone, even though
          -- public/ still has a copy.
          removeFile (dir </> "a-annot.json")
          status "GET" "a-annot.json" "" `shouldReturn` 404
          status "HEAD" "a-annot.json" "" `shouldReturn` 404

          -- Control: editing the deck does build, which prunes the copy.
          T.appendFile (dir </> "a-deck.md") "\nMore\n"
          waitUntil 60 (not <$> doesFileExist (public "a-annot.json"))
            `shouldReturn` True

-- | Runs @decker --server@ on the project and passes a request function to
-- the action once the server answers.
withServer :: FilePath -> (([Header] -> String -> String -> LBS.ByteString -> IO (Response LBS.ByteString)) -> IO a) -> IO a
withServer dir action = do
  exe <- deckerExecutable
  port <- getFreePort
  manager <- newManager defaultManagerSettings
  let requestWith headers method path body = do
        req <- parseRequest ("http://127.0.0.1:" <> show port <> "/" <> path)
        httpLbs
          req
            { method = BS.pack method,
              requestHeaders = headers,
              requestBody = RequestBodyLBS body
            }
          manager
      request = requestWith []
      args = ["--project-dir=" <> dir, "--server", "--port=" <> show port]
  withFile "/dev/null" WriteMode $ \null ->
    bracket
      (createProcess (proc exe args) {std_out = UseHandle null, std_err = UseHandle null, std_in = NoStream})
      (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)
      ( \_ -> do
          up <- waitUntil 120 $ do
            answer <- try (request "GET" "a-deck.html" "") :: IO (Either SomeException (Response LBS.ByteString))
            return $ either (const False) ((== 200) . statusCode . responseStatus) answer
          up `shouldBe` True
          action requestWith
      )

-- | Polls the condition every 250 ms, for at most the given seconds.
waitUntil :: Int -> IO Bool -> IO Bool
waitUntil seconds condition = go (seconds * 4)
  where
    go 0 = condition
    go n = do
      ok <- condition
      if ok then return True else threadDelay 250000 >> go (n - 1 :: Int)

-- | The backup files of the project's server sessions with their contents.
backupsOf :: FilePath -> IO [(FilePath, String)]
backupsOf dir = do
  temp <- getTemporaryDirectory
  let root = mkTmpDirName temp dir </> "backup"
  exists <- doesDirectoryExist root
  files <- if exists then listFiles root else return []
  forM files $ \file -> (file,) <$> readFile file
  where
    listFiles d = do
      entries <- map (d </>) <$> listDirectory d
      dirs <- filterM doesDirectoryExist entries
      files <- filterM doesFileExist entries
      nested <- concat <$> mapM listFiles dirs
      return (files <> nested)
