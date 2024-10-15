{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Index (buildIndex) where

import Data.Aeson
import Data.Char
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Development.Shake hiding (Resource)
import GHC.Generics hiding (Meta)
import Relude
import System.FilePath
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Meta
import Text.Decker.Reader.Markdown
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Shared
import Text.Pandoc.Walk

-- For lookup use: http://glench.github.io/fuzzyset.js/

-- | Computes the inverted index over all decks and stores it in JSON format.
-- | Also returns a list of non-draft sources.
buildIndex :: FilePath -> Meta -> [FilePath] -> Action [FilePath]
buildIndex indexFile globalMeta decks = do
  need decks
  index <- catMaybes <$> mapM (buildDeckIndex globalMeta) decks
  let inverted = invertIndex index
  liftIO $ encodeFile indexFile inverted
  return $ map (deckSrc . fst) index

-- | Only index decks which are not marked `draft` and are not in the `no-index`
-- list 
shouldAddToIndex meta =
  let deckId :: Text = lookupMetaOrElse "" "feedback.deck-id" meta
      noIndex = lookupMetaOrElse [] "no-index" meta
      isDraft =
        lookupMetaOrElse False "draft" meta
          || lookupMeta "lecture.status" meta
          == Just ("draft" :: Text)
   in not isDraft && (deckId `notElem` noIndex)

-- Collects word frequencies for each slide grouped by deck.
buildDeckIndex :: Meta -> FilePath -> Action (Maybe (DeckInfo, [((Text, Text), [(Text, Int)])]))
buildDeckIndex globalMeta path = do
  pandoc@(Pandoc meta blocks) <-
    readMarkdownFile globalMeta path >>= mergeDocumentMeta globalMeta
  if shouldAddToIndex meta
    then do
      let deckTitle = lookupMeta "title" meta
      let deckSubtitle = lookupMeta "subtitle" meta
      let deckId = lookupMeta "deckId" meta
      let deckSrc = path
      let deckUrl = toText (path -<.> "html")
      -- Take out the empty ones for now
      let deckIndex =
            map (first fromJust)
              $ filter (isJust . fst)
              $ mapSlides indexSlide pandoc
      putNormal $ "# indexing (" <> path <> ")"
      return $ Just (DeckInfo {deckSrc, deckUrl, deckId, deckTitle, deckSubtitle}, deckIndex)
    else do
      putNormal $ "# skip indexing (" <> path <> ")"
      return Nothing

-- Extracts all searchable words from an inline
extractInlineWords :: Inline -> [Text]
extractInlineWords (Str text) = sanitizeText text
extractInlineWords (Code _ text) = sanitizeText text
extractInlineWords _ = []

-- Extracts all searchable words from an inline
extractBlockWords :: Block -> [Text]
extractBlockWords (CodeBlock _ text) = sanitizeText text
extractBlockWords _ = []

-- Converts a list of sane words.
sanitizeText :: Text -> [Text]
sanitizeText =
  filter ((3 <=) . Text.length)
    . words
    . Text.toLower
    . Text.map (\c -> if isAlphaNum c then c else ' ')

-- Calculates the frequency of list elements.
frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

-- Extracts all words from a slide.
indexSlide :: Slide -> (Maybe (Text, Text), [(Text, Int)])
indexSlide slide@(Slide header body dir) =
  let words =
        maybe [] (query extractInlineWords) header
          <> query extractInlineWords body
          <> query extractBlockWords body
   in (extractIdTitle slide, frequency words)

-- Maps f over all slides in a Pandoc document.
mapSlides :: (Slide -> a) -> Pandoc -> [a]
mapSlides f (Pandoc meta blocks) =
  -- TODO: force normalisation of slide separation and ids for emtpy headers
  map f (toSlides blocks)

-- Extracts id and title from slide header if it has one.
extractIdTitle :: Slide -> Maybe (Text, Text)
extractIdTitle (Slide (Just (Header _ (id, _, _) text)) _ _) =
  Just (id, stringify text)
extractIdTitle _ = Nothing

type SlideUrl = Text

type DeckUrl = Text

data SlideRef = SlideRef
  { slide :: SlideUrl,
    count :: Int
  }
  deriving (Generic, Show)

data DeckInfo = DeckInfo
  { deckSrc :: FilePath,
    deckUrl :: DeckUrl,
    deckId :: Maybe Text,
    deckTitle :: Maybe Text,
    deckSubtitle :: Maybe Text
  }
  deriving (Generic, Show)

data SlideInfo = SlideInfo
  { slideUrl :: SlideUrl,
    slideId :: Text,
    slideTitle :: Text,
    deckUrl :: DeckUrl
  }
  deriving (Generic, Show)

type WordMap = Map Text [SlideRef]

type SlideMap = Map SlideUrl SlideInfo

type DeckMap = Map DeckUrl DeckInfo

data Index = Index
  { index :: WordMap,
    slides :: SlideMap,
    decks :: DeckMap
  }
  deriving (Generic, Show)

instance ToJSON SlideRef

instance ToJSON DeckInfo

instance ToJSON SlideInfo

instance ToJSON Index

-- | Inverts the index. Deck and slide info is store in seperate maps and can
-- be referenced by the respective URLs.
invertIndex :: [(DeckInfo, [((Text, Text), [(Text, Int)])])] -> Index
invertIndex =
  foldl'
    ( \index (deckInfo@(DeckInfo deckSrc deckUrl _ _ _), slides) ->
        foldl'
          ( \index ((slideId, slideTitle), words) ->
              foldl'
                ( \(Index wordMap slideMap deckMap) (word, count) ->
                    let slideUrl = deckUrl <#> slideId
                     in Index
                          (insertWord word (SlideRef slideUrl count) wordMap)
                          ( insertIfMissing
                              slideUrl
                              SlideInfo
                                { slideUrl,
                                  slideId,
                                  slideTitle,
                                  deckUrl
                                }
                              slideMap
                          )
                          (insertIfMissing deckUrl deckInfo deckMap)
                )
                index
                words
          )
          index
          slides
    )
    emptyIndex

-- | Inserts value with key if key does not yet exist.
insertIfMissing :: (Ord k) => k -> a -> Map k a -> Map k a
insertIfMissing k v m = if Map.member k m then m else Map.insert k v m

(<#>) :: (Semigroup a, IsString a) => a -> a -> a
a <#> b = a <> "#" <> b

emptyIndex :: Index
emptyIndex = Index (fromList []) (fromList []) (fromList [])

-- | Inserts a word with slide reference into the word map. Multiple slide refs
-- for a word are accumulated in a list. This is basically a multi-map.
insertWord :: Text -> SlideRef -> WordMap -> WordMap
insertWord word entry = Map.alter add word
  where
    add Nothing = Just [entry]
    add (Just list) = Just (entry : list)
