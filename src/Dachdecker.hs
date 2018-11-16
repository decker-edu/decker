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
import Data.Aeson.Lens (_Bool, _Integer, _Object, _String, key, nth, values)
import qualified Data.ByteString.Char8 as BSC
import Data.List (isPrefixOf)
import Data.Map
import Data.Text
import qualified Data.Text.IO as T
import Development.Shake (Action, liftIO)
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Project
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit
import System.FilePath
import System.IO
import Text.Pandoc
import Utilities

uploadQuizzes :: Action [FilePath] -> Action ()
uploadQuizzes markdownDecks
  -- TODO need the generated html files
 = do
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
  -- Find deck id from the meta data or exit if none is present
  -- deckId <-
  case dachdeckerFromMeta meta of
    Just deckId -> do
      uploadMarkdown deckId path token
      uploadPublic deckId token
      issueSurveyExtraction deckId token
      return ()
    Nothing -> do
      putStrLn $ "Unable to find Deck ID in file " ++ path
      putStrLn "Please provide one in your yaml header"
      return ()
        -- Just deckId' <- createDeck token -- TODO, "Nothing" is ignored here
        -- putStr "Created new Deck ID: "
        -- putStrLn deckId'
        -- let meta' =
        --       Meta $ insert "dachdecker" (MetaString deckId') (unMeta meta)
        -- -- let writerOptions = def {writerExtensions = enableExtension Ext_yaml_metadata_block pandocExtensions}
        -- let writerOptions =
        --       def
        --         { writerExtensions =
        --             extensionsFromList [Ext_yaml_metadata_block]
        --         }
        -- print writerOptions
        -- markdownContent <-
        --   runIO (writeMarkdown writerOptions (Pandoc meta' content)) >>=
        --   handleError
        -- -- writeMarkdown doesn't include the yaml header, therefore 
        -- -- destroying the file upon write
        -- writeFile path (Data.Text.unpack markdownContent)
        -- return deckId'

-- | Suppresses echo when prompting for a password
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

-- | Uploads the Markdown file for a deck
-- The markdown file is special as there is only one
-- entry file for a presentation.
-- Additional markdown files that may get included upon
-- build are ignored for now.
uploadMarkdown :: String -> FilePath -> String -> IO ()
uploadMarkdown deckId path token
  -- TODO let Pandoc parse the Markdown file
  -- and find other Markdown files that get included.
  -- Upload them as well
 = do
  projectDirs <- projectDirectories
  uploadFile deckId token path (makeRelativeTo (project projectDirs) path) False
  return ()

uploadPublic :: String -> String -> IO ()
uploadPublic deckId token = do
  projectDirs <- projectDirectories
  let publicDir = public projectDirs
  uploadPublicRecursive publicDir ""
  where
    uploadPublicRecursive :: FilePath -> String -> IO ()
    uploadPublicRecursive src uploadPath = do
      isDir <- doesDirectoryExist src
      let (_, fileName) = splitFileName src
      if isDir && ("support-" `Data.List.isPrefixOf` fileName)
        then do
          return ()
        else if isDir
               then do
                 contents <- listDirectory src
                 mapM
                   (\x ->
                      uploadPublicRecursive (src </> x) (uploadPath ++ "/" ++ x))
                   contents
                 return ()
               else do
                 uploadFile deckId token src uploadPath True

-- | Uploads a file from the public folder to the server
-- First asks the server to upload the file. The server answers with an id
-- to upload to. Then the file is uploaded.
uploadFile ::
     String -- ^ ID of the deck
  -> String -- ^ authentication token
  -> FilePath -- ^ local file path relative to either the public or the project directory
  -> String -- ^ path that is reported to the server
  -> Bool -- ^ indicates if the file is processed, i.e., in the public directory
  -> IO ()
uploadFile deckId token localPath uploadPath processed = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  r <-
    postWith
      opts
      (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file")
      (toJSON (object ["path" .= uploadPath, "processed" .= processed]))
  let Just uploadId = r ^? responseBody . key "id" . _Integer
  ru <-
    postWith
      opts
      (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file/" ++ (show uploadId))
      (partFile "file" localPath)
  return ()

-- | Ask the server to update the surveys based on the uploaded files
issueSurveyExtraction ::
     String -- ^ ID of the deck
  -> String -- ^ authentication token
  -> IO ()
issueSurveyExtraction deckId token = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  putWith
    opts
    (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/survey")
    (toJSON (object ["generate" .= True]))
  return ()
