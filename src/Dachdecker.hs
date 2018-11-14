{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Dachdecker
  ( login
  , uploadQuizzes
  ) where

import Context
import Control.Exception
import Control.Lens ((&), (.~), (?~), (^?))
import Data.Aeson ((.=), fromJSON, object, toJSON)
import Data.Aeson.Lens (_Bool, _Object, _String, key, nth, values)
import qualified Data.ByteString.Char8 as BSC
import Data.Map
import Data.Text
import qualified Data.Text.IO as T
import Development.Shake
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Project
import System.IO
import Text.Pandoc
import Utilities

uploadQuizzes :: Action [FilePath] -> Action ()
uploadQuizzes markdownDecks = do
  -- TODO need the generated html files
  d <- markdownDecks
  liftIO $ uploadQuizzesIO d
  return ()

uploadQuizzesIO :: [FilePath] -> IO ()
uploadQuizzesIO deckPaths = do
  putStrLn "Synchronizing your presentation with the server"
  putStr "The upload will include all files necessary for your presentation. "
  putStrLn "Depending on your content, this may take some time."
  putStrLn "Please enter your server credentials"
  putStr "Server: "
  getDachdeckerUrl >>= putStrLn
  putStr "Username: "
  hFlush stdout
  username <- getLine
  putStr "Password: "
  hFlush stdout
  password <- withEcho False getLine
  putStrLn ""
  maybeToken <- login username password
  case maybeToken of
    Just token -> mapM (uploadQuiz token) deckPaths
    Nothing -> do
      return [()]
  return ()

uploadQuiz :: String -> FilePath -> IO ()
uploadQuiz token path = do
  markdown <- T.readFile path
  let Pandoc meta content = readMarkdownOrThrow pandocReaderOpts markdown
  -- Find deck id from the meta data or create a new deck if none is present
  deckId <-
    case dachdeckerFromMeta meta of
      Just deckId' -> do
        return deckId'
      Nothing -> do
        Just deckId' <- createDeck token -- TODO, "Nothing" is ignored here
        let meta' =
              Meta $ insert "dachdecker" (MetaString deckId') (unMeta meta)
        markdownContent <-
          runIO (writeMarkdown def (Pandoc meta' content)) >>= handleError
        writeFile path (Data.Text.unpack markdownContent)
        return deckId'
  uploadMarkdown deckId path token
  return ()

-- Copied from
-- https://stackoverflow.com/questions/4064378/prompting-for-a-password-in-haskell-command-line-application
withEcho :: Bool -> IO a -> IO a
withEcho echo action =
  bracket
    (hGetEcho stdin)
    (hSetEcho stdin)
    (const $ hSetEcho stdin echo >> action)

-- | Login to the Dachdecker server
-- Use @user@ and @pass@ as credentials to log in
-- The Dachdecker server allows to hold authentication status
-- with either Cookies or an authentication token
login :: String -> String -> IO (Maybe String)
login user pass = do
  baseUrl <- getDachdeckerUrl
  r <-
    post
      (baseUrl ++ "/api/v1/user/login")
      (toJSON (object ["name" .= user, "password" .= pass]))
  let loginSuccess = r ^? responseBody . key "login" . _Bool
  let token = r ^? responseBody . key "token" . _String
  return (fmap Data.Text.unpack token)

-- | Creates a new deck entry on the server
-- With the authentication token, the server is asked to
-- create a new deck entry where files can be uploaded
-- and surveys can get registered. Returns the deckId
-- used to work with the deck if the server returns it
-- otherwise there is an error
createDeck :: String -> IO (Maybe String)
createDeck token = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  r <- postWith opts (baseUrl ++ "/api/v1/deck") (toJSON ())
  let id = r ^? responseBody . key "id" . _String
  return (fmap Data.Text.unpack id)

uploadMarkdown :: String -> FilePath -> String -> IO ()
uploadMarkdown deckId path token = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  r <-
    postWith opts (baseUrl ++ "/api/v1/deck/" ++ deckId) (partFile "deck" path)
  return ()

-- | Uploads a file from the public folder to the server
-- First asks the server to upload the file. The server answers with an id
-- to upload to. Then the file is uploaded.
uploadFile :: String -> String -> (FilePath, String) -> IO ()
uploadFile deckId token (localPath, uploadPath) = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  r <-
    postWith
      opts
      (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file")
      (toJSON (object ["path" .= uploadPath, "processed" .= True]))
  let Just uploadId = r ^? responseBody . key "id" . _String
  ru <-
    postWith
      opts
      (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file/" ++ (unpack uploadId))
      (partFile "file" localPath)
  return ()
