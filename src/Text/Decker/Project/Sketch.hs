module Text.Decker.Project.Sketch
  ( randomId
  , writeToMarkdownFile
  , provideSlideIds
  , provideSlideIdIO
  , idDigits
  ) where

import Text.Decker.Filter.Slide
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Writer.Markdown
import Text.Pandoc.Lens

import Control.Monad
import qualified Data.Text.IO as T
import Development.Shake
import System.Directory
import System.FilePath
import System.Random
import Text.Pandoc hiding (writeMarkdown)

idDigits = 4

-- | Selects a random id out of idDigits^36 possibilities
randomId :: IO String
randomId = do
  h <- randomAlpha
  t <- replicateM (idDigits - 1) randomAlphaNum
  return $ h : t

-- | Rejection sampling for a random character from [0-9] or [a-z].
randomAlphaNum :: IO Char
randomAlphaNum = do
  r <- getStdRandom (randomR ('0', 'z'))
  if r > '9' && r < 'a'
    then randomAlphaNum
    else return r

randomAlpha :: IO Char
randomAlpha = getStdRandom (randomR ('a', 'z'))

-- | Writes a pandoc document atoimically to a markdown file. It uses a modified
-- Markdown writer that produces more appropriately formatted documents.
writeToMarkdownFile :: FilePath -> Pandoc -> Action ()
writeToMarkdownFile filepath pandoc@(Pandoc pmeta _) = do
  template <- liftIO $ getResourceString $ "template" </> "deck.md"
  let columns = lookupInt "write-back.line-columns" 80 pmeta
  let wrap = lookupString "write-back.line-wrap" "" pmeta
  let wrapOpt "none" = WrapNone
      wrapOpt "preserve" = WrapPreserve
      wrapOpt _ = WrapAuto
  let wrap = lookupString "write-back.line-wrap" "none" pmeta
  let extensions =
        (disableExtension Ext_simple_tables .
         disableExtension Ext_multiline_tables .
         enableExtension Ext_auto_identifiers)
          pandocExtensions
  let options =
        def
          { writerTemplate = Just template
          , writerExtensions = extensions
          , writerColumns = columns
          , writerWrapText = wrapOpt wrap
          , writerSetextHeaders = False
          }
  markdown <- liftIO $ runIO (writeMarkdown options pandoc) >>= handleError
  fileContent <- liftIO $ T.readFile filepath
  when (markdown /= fileContent) $
    withTempFile
      (\tmp -> liftIO $ T.writeFile tmp markdown >> renameFile tmp filepath)

provideSlideIds :: Pandoc -> IO Pandoc
provideSlideIds (Pandoc meta body) = do
  let slides = toSlides body
  idSlides <- mapAccumM provideSlideIdIO ["decker-title-slide"] slides
  let idBody = fromSlides idSlides
  return $ Pandoc meta idBody

-- Provides unique, random, sticky ids for all slides.
-- provideSlideId :: Slide -> Decker Slide
-- provideSlideId = doIO . provideSlideIdIO
provideSlideIdIO :: [String] -> Slide -> IO ([String], Slide)
-- Create a new random ID if there is none
-- Create a new random ID if the existing one is not unique
-- Preserve an existing unique ID
provideSlideIdIO ids slide@(Slide (Just (Header 1 (sid, c, kv) i)) body) =
  if sid == "" || sid `elem` ids
    then do
      sid <- randomId
      return (sid : ids, Slide (Just $ Header 1 (sid, c, kv) i) body)
    else return (sid : ids, slide)
-- Create a random ID and an empty level 1 header if there is neither
provideSlideIdIO ids (Slide Nothing body) = do
  sid <- randomId
  return (sid : ids, Slide (Just $ Header 1 (sid, [], []) []) body)

-- Monadic map with an accumulator
mapAccumM :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m [y]
mapAccumM _ z [] = return []
mapAccumM f z (x:xs) = do
  (z', y) <- f z x
  ys <- mapAccumM f z' xs
  return (y : ys)
