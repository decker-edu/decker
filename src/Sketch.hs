module Sketch
  ( randomId
  , writeToMarkdownFile
  , provideSlideIds
  , provideSlideId
  , provideSlideIdIO
  , idDigits
  ) where

import Common
import Resources
import Slide

import Control.Monad
import qualified Data.Text.IO as T
import System.FilePath
import System.Random
import Text.Pandoc
import Text.Pandoc.Shared

idDigits = 4

-- | Selects a random id out of idDigits^36 possibilities
randomId :: IO String
randomId = ('s' :) <$> replicateM idDigits randomChar

-- | Rejection sampling for a random character from [0-9] or [a-z].
randomChar :: IO Char
randomChar = do
  r <- getStdRandom (randomR ('0', 'z'))
  if r > '9' && r < 'a'
    then randomChar
    else return r

-- | Writes a pandoc document to a markdown file.
writeToMarkdownFile :: FilePath -> Pandoc -> IO ()
writeToMarkdownFile filepath pandoc
  -- putStrLn $ "Writing back: " ++ filepath
 = do
  template <- getResourceString $ "template" </> "deck.md"
  let extensions =
        (disableExtension Ext_simple_tables .
         disableExtension Ext_multiline_tables .
         disableExtension Ext_grid_tables . enableExtension Ext_auto_identifiers)
          pandocExtensions
  let options =
        def
          { writerTemplate = Just template
          , writerExtensions = extensions
          , writerWrapText = WrapAuto
          , writerColumns = 999
          , writerSetextHeaders = False
          }
  runIO (writeMarkdown options pandoc) >>= handleError >>= T.writeFile filepath

provideSlideIds :: Pandoc -> IO Pandoc
provideSlideIds (Pandoc meta body) = do
  let slides = toSlides body
  idSlides <- mapM provideSlideIdIO slides
  let idBody = fromSlides idSlides
  return $ Pandoc meta idBody

-- Provides unique, random, sticky ids for all slides.
provideSlideId :: Slide -> Decker Slide
provideSlideId = doIO . provideSlideIdIO

provideSlideIdIO :: Slide -> IO Slide
provideSlideIdIO (Slide (Just (Header 1 ("", c, kv) i)) body) = do
  sid <- randomId
  -- print (Slide (Just $ Header 1 (sid, c, kv) i) body)
  return $ Slide (Just $ Header 1 (sid, c, kv) i) body
provideSlideIdIO (Slide Nothing body) = do
  sid <- randomId
  return $ Slide (Just $ Header 1 (sid, [], []) []) body
provideSlideIdIO slide@(Slide (Just (Header 1 (sid, c, kv) i)) body)
  -- print (Slide (Just $ Header 1 (sid, c, kv) i) body)
 = do
  return slide
