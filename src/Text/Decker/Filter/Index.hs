{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Index where

import Data.Aeson
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
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

buildIndex :: FilePath -> Meta -> [FilePath] -> Action ()
buildIndex indexFile globalMeta decks = do
  index <- mapM (buildDeckIndex globalMeta) decks
  let inverted = invertIndex index
  liftIO $ encodeFile indexFile inverted

buildDeckIndex :: Meta -> FilePath -> Action (DeckInfo, [((Text, Text), [(Text, Int)])])
-- Take out the empty ones for now
buildDeckIndex globalMeta path = do
  pandoc@(Pandoc meta blocks) <- readMarkdownFile globalMeta path
  let deckTitle = lookupMeta "title" meta
  let deckId = lookupMeta "deckId" meta
  let deckUrl = toText (path -<.> "html")
  let deckIndex = map (first fromJust) $ filter (isJust . fst) $ mapSlides indexSlide pandoc
  return (DeckInfo {deckUrl, deckId, deckTitle}, deckIndex)

-- TODO make this more elaborate
stringi :: Inline -> Text
stringi = stringify

onlyAlphaNum :: Text -> Text
onlyAlphaNum = Text.map (\c -> if isAlphaNum c then c else ' ')

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

indexSlide :: Slide -> (Maybe (Text, Text), [(Text, Int)])
indexSlide slide@(Slide header body) =
  let headerText = onlyAlphaNum $ maybe "" (query stringi) header
      bodyText = onlyAlphaNum $ query stringi body
      all = words headerText <> words bodyText
      filtered = map Text.toLower $ filter ((3 <=) . Text.length) all
   in (extractId slide, frequency filtered)

mapSlides :: (Slide -> a) -> Pandoc -> [a]
mapSlides f (Pandoc meta blocks) =
  -- TODO force normalisation of slide separation and ids for emtpy headers
  map f (toSlides blocks)

extractId :: Slide -> Maybe (Text, Text)
extractId (Slide (Just (Header _ (id, _, _) text)) _) = Just (id, stringify text)
extractId _ = Nothing

data DeckInfo = DeckInfo
  { deckUrl :: Text,
    deckId :: Maybe Text,
    deckTitle :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON DeckInfo

data SlideInfo = SlideInfo
  { slideUrl :: SlideUrl,
    slideId :: Text,
    slideTitle :: Text,
    deckUrl :: Text,
    deckId :: Maybe Text,
    deckTitle :: Maybe Text
  }
  deriving (Generic, Show)

instance ToJSON SlideInfo

instance FromJSON SlideInfo

type SlideUrl = Text

type SlideMap = Map SlideUrl SlideInfo

data SlideRef = SlideRef
  { slide :: SlideUrl,
    count :: Int
  }
  deriving (Generic, Show)

instance ToJSON SlideRef

type WordMap = Map Text [SlideRef]

data Index = Index
  { index :: WordMap,
    slides :: SlideMap
  }
  deriving (Generic, Show)

instance ToJSON Index

invertIndex :: [(DeckInfo, [((Text, Text), [(Text, Int)])])] -> Index
invertIndex =
  foldl'
    ( \index (DeckInfo deckUrl deckId deckTitle, slides) ->
        foldl'
          ( \index ((slideId, slideTitle), words) ->
              let slideUrl = deckUrl <#> slideId
               in foldl'
                    ( \(Index wordMap slideMap) (word, count) ->
                        Index
                          (insertWord word (SlideRef slideUrl count) wordMap)
                          ( insertIfMissing
                              slideUrl
                              SlideInfo
                                { slideUrl,
                                  slideId,
                                  slideTitle,
                                  deckUrl,
                                  deckId,
                                  deckTitle
                                }
                              slideMap
                          )
                    )
                    index
                    words
          )
          index
          slides
    )
    emptyIndex

insertIfMissing :: Ord k => k -> a -> Map k a -> Map k a
insertIfMissing k v m = if Map.member k m then m else Map.insert k v m

(<#>) :: (Semigroup a, IsString a) => a -> a -> a
a <#> b = a <> "#" <> b

emptyIndex :: Index
emptyIndex = Index (fromList []) (fromList [])

insertWord :: Text -> SlideRef -> WordMap -> WordMap
insertWord word entry = Map.alter add word
  where
    add Nothing = Just [entry]
    add (Just list) = Just (entry : list)
