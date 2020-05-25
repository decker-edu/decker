{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Server.Dachdecker
  ( login
  , uploadQuizzes
  ) where

import Text.Decker.Internal.Exception
import Text.Decker.Project.Project

import Control.Exception
import Control.Lens ((&), (.~), (^.), (^?))
import Data.Aeson ((.=), object, toJSON)
import Data.Aeson.Lens (_Integer, _String, key)
import qualified Data.ByteString.Char8 as BSC
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Text
import qualified Data.Text.IO as T
import Development.Shake (Action, liftIO)
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Network.Wreq
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath
import System.IO
import Text.Pandoc

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
      putStrLn "Couldn't upload to the server"
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
-- Use @user@ and @pass@ as credentials to log in.
-- Authentication is done with the university Gitlab server.
-- The resulting token is sent to the Dachdecker server
-- to prove a successful authentication.
-- The Dachdecker server allows to hold authentication status
-- with either Cookies or an authentication token
login :: String -> String -> IO (Maybe String)
login user pass = do
  gitlabResponse <-
    catch
      ((post
          "https://gitlab2.informatik.uni-wuerzburg.de/oauth/token"
          [ "grant_type" := ("password" :: String)
          , "username" := user
          , "password" := pass
          ]) <&>
       Just)
      handler
  case gitlabResponse of
    Just gitlabResult -> do
      let token =
            (gitlabResult ^? responseBody . key "access_token" . _String) <&>
            Data.Text.unpack
      case token of
        Just gitlabToken -> do
          baseUrl <- getDachdeckerUrl
          dachdeckerResponse <-
            catch
              ((get (baseUrl ++ "/api/v1/user/login?token=" ++ gitlabToken)) <&>
               Just)
              handler
          case dachdeckerResponse of
            Just dachdeckerResult -> do
              return $
                (dachdeckerResult ^? responseBody . key "token" . _String) <&>
                Data.Text.unpack
            Nothing -> do
              return Nothing
        Nothing -> do
          return Nothing
    Nothing -> do
      return Nothing
  where
    handler (HttpExceptionRequest e f) = do
      putStrLn "Invalid credentials"
      return Nothing

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
  uploadFile
    deckId
    token
    path
    (makeRelativeTo (projectDirs ^. project) path)
    False
  return ()

uploadPublic :: String -> String -> IO ()
uploadPublic deckId token = do
  dirs <- projectDirectories
  let publicDir = dirs ^. public
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
                      uploadPublicRecursive
                        (src </> x)
                        (if uploadPath /= ""
                           then uploadPath ++ "/" ++ x
                           else x))
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
  dachdeckerUploadRequestResponse <-
    (catch
       ((postWith
           opts
           (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file")
           (toJSON (object ["path" .= uploadPath, "processed" .= processed]))) <&>
        Just)
       handler)
  let uploadId =
        case dachdeckerUploadRequestResponse of
          Just response -> response ^? responseBody . key "id" . _Integer
          Nothing -> Nothing
  uploadSuccess <-
    case uploadId of
      Just uploadId' ->
        catch
          ((postWith
              opts
              (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/file/" ++
               (show uploadId'))
              (partFile "file" localPath)) <&>
           Just)
          handler
      Nothing -> do
        return Nothing
  -- putStrLn $ "Upload file: " ++ localPath ++ " -> " ++ uploadPath ++ " - " ++ show (isJust uploadSuccess)
  return ()
  where
    handler (HttpExceptionRequest e f) = do
      putStrLn $ "Error uploading " ++ show (localPath)
      return Nothing

-- | Ask the server to update the surveys based on the uploaded files
issueSurveyExtraction ::
     String -- ^ ID of the deck
  -> String -- ^ authentication token
  -> IO ()
issueSurveyExtraction deckId token = do
  baseUrl <- getDachdeckerUrl
  let opts = defaults & header "X-Auth-Token" .~ [BSC.pack token]
  catch
    ((putWith
        opts
        (baseUrl ++ "/api/v1/deck/" ++ deckId ++ "/survey")
        (toJSON (object ["generate" .= True]))) <&>
     Just)
    handler
  return ()
  where
    handler (HttpExceptionRequest e f) = do
      putStrLn $ "Error updating quizzes"
      return Nothing

------------------------------------------
------------------------- Copies of functions from utilities to break cycles ---
------------------------------------------
readMarkdownOrThrow :: ReaderOptions -> Text -> Pandoc
readMarkdownOrThrow opts markdown =
  case runPure (readMarkdown opts markdown) of
    Right pandoc -> pandoc
    Left errMsg -> throw $ PandocException (show errMsg)

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism if slides have duplicate titles in separate
-- include files.
deckerPandocExtensions :: Extensions
deckerPandocExtensions =
  (disableExtension Ext_auto_identifiers . disableExtension Ext_simple_tables .
   disableExtension Ext_multiline_tables)
    pandocExtensions

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def {readerExtensions = deckerPandocExtensions}
