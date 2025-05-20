{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Text.Decker.Filter.Index (buildIndex, readDeckInfo, renderIndex) where

import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BS
import Data.Char
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Development.Shake hiding (Resource)
import GHC.Generics hiding (Meta)
import Relude
import System.FilePath
import Text.Decker.Filter.Slide
import Text.Decker.Filter.Util (hash9String)
import Text.Decker.Internal.Common (publicDir, privateDir)
import Text.Decker.Internal.Helper (makeRelativeTo)
import Text.Decker.Internal.Meta
import Text.Decker.Internal.MetaExtra (mergeDocumentMeta)
import Text.Decker.Project.Project qualified as Project
import Text.Decker.Project.Shake (relativeSupportDir)
import Text.Decker.Reader.Markdown
import Text.Decker.Writer.CSS (computeCssColorVariables, computeCssVariables)
import Text.DocLayout (render)
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Decker.Exam.Question

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
      let deckAuthor = lookupMeta "author" meta
      let deckDate = lookupMeta "date" meta
      let deckSrc = path
      let deckUrl = toText (path -<.> "html")
      -- Take out the empty ones for now
      let deckIndex =
            map (first fromJust)
              $ filter (isJust . fst)
              $ mapSlides indexSlide pandoc
      putNormal $ "# indexing (" <> path <> ")"
      return $ Just (DeckInfo {deckSrc, deckUrl, deckId, deckAuthor, deckDate, deckTitle, deckSubtitle}, deckIndex)
    else do
      putNormal $ "# skip indexing (" <> path <> ")"
      return Nothing

readDeckInfo :: Meta -> (FilePath, FilePath) -> Action DeckInfo
readDeckInfo globalMeta (target, src) = do
  pandoc@(Pandoc meta blocks) <-
    readMarkdownFile globalMeta src >>= mergeDocumentMeta globalMeta
  let deckTitle = lookupMeta "title" meta
  let deckSubtitle = lookupMeta "subtitle" meta
  let deckId = lookupMeta "deckId" meta
  let deckAuthor = lookupMeta "author" meta
  let deckDate = lookupMeta "date" meta
  let deckSrc = src
  let deckUrl = toText $ makeRelativeTo publicDir target
  return $ DeckInfo {deckSrc, deckUrl, deckId, deckAuthor, deckDate, deckTitle, deckSubtitle}

readQuestInfo :: Meta -> (FilePath, FilePath) -> Action QuestInfo
readQuestInfo globalMeta (target, src) = do
  Question topicId lectureId title _ _ _ _ comment _ _ _ _ <- liftIO $ readQuestion src
  let questSrc = src
  let questUrl = toText $ makeRelativeTo privateDir target
  let questLectureId = lectureId
  let questTopicId = topicId
  let questTitle = title
  let questComment = comment
  return $ QuestInfo { questSrc ,questUrl ,questLectureId ,questTopicId ,questTitle ,questComment }

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
    deckAuthor :: Maybe Text,
    deckDate :: Maybe Text,
    deckTitle :: Maybe Text,
    deckSubtitle :: Maybe Text
  }
  deriving (Generic, Show)

data QuestInfo = QuestInfo
  { questSrc :: FilePath,
    questUrl :: Text,
    questLectureId :: Text,
    questTopicId :: Text,
    questTitle :: Text,
    questComment :: Text
  }
  deriving (Generic, Show)

data SlideInfo = SlideInfo
  { slideUrl :: SlideUrl,
    slideId :: Text,
    slideTitle :: Text,
    slideDeckUrl :: DeckUrl
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
    ( \index (deckInfo@(DeckInfo deckSrc deckUrl _ _ _ _ _), slides) ->
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
                                  slideDeckUrl = deckUrl
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

renderIndex :: Template Text -> Meta -> Project.Targets -> FilePath -> Action ()
renderIndex template meta targets out = do
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let metaFile = hash9String out <.> ".json"
  let metaPath = takeDirectory out </> metaFile
  targetMeta <-
    addTargetInfo targets
      $ setMetaValue "decker-meta-url" (toText metaFile)
      $ setMetaValue "decker-support-dir" (toText relSupportDir)
      $ computeCssColorVariables
      $ computeCssVariables meta
  let aesonMeta = fromPandocMeta targetMeta
  let text :: Text = render Nothing $ renderTemplate template aesonMeta
  liftIO $ Text.writeFile out text
  let jsonMeta = encodePretty aesonMeta
  liftIO $ BS.writeFile metaPath jsonMeta

addTargetInfo :: Project.Targets -> Meta -> Action Meta
addTargetInfo targets meta = do
  let allDecks = getSorted Project.decks
  let allPages = getSorted Project.pages
  let allQuests = getSorted Project.questions
  decksInfo <- mapM (readDeckInfo meta) allDecks
  pagesInfo <- mapM (readDeckInfo meta) allPages
  questInfo <- mapM (readQuestInfo meta) allQuests
  let withDecks =
        setMetaValue "decks.by-title" (toListSortedBy deckTitle decksInfo)
          $ setMetaValue "decks.by-date" (toListSortedBy deckDate decksInfo)
          $ setMetaValue "decks.by-url" (toListSortedBy deckUrl decksInfo)
          $ setMetaValue "decks.by-author" (toListSortedBy deckAuthor decksInfo)
          $ setMetaValue "decks.by-id" (toListSortedBy deckId decksInfo) meta
  let withPagesAndDecks =
        setMetaValue "pages.by-title" (toListSortedBy deckTitle pagesInfo)
          $ setMetaValue "pages.by-date" (toListSortedBy deckDate pagesInfo)
          $ setMetaValue "pages.by-url" (toListSortedBy deckUrl pagesInfo)
          $ setMetaValue "pages.by-author" (toListSortedBy deckAuthor pagesInfo)
          $ setMetaValue "pages.by-id" (toListSortedBy deckId pagesInfo) withDecks
  let withPagesDecksAndQuests =
        setMetaValue "quests.by-title" (toQuestListSortedBy questTitle questInfo)
        $ setMetaValue "quests.by-url" (toQuestListSortedBy questUrl questInfo)
        $ setMetaValue "quests.by-lecture-id" (toQuestListSortedBy questLectureId questInfo)
        $ setMetaValue "quests.by-topic-id" (toQuestListSortedBy questTopicId questInfo) withPagesAndDecks
  return withPagesDecksAndQuests
  where
    toQuestListSortedBy by info = MetaList $ map toQuestMeta $ sortInfo by info
    toListSortedBy by info = MetaList $ map toMeta $ sortInfo by info
    getSorted field = sort $ Map.toList (targets ^. field)
    toMeta :: DeckInfo -> MetaValue
    toMeta info =
      MetaMap
        $ fromList
          [ ("src", MetaString (toText info.deckSrc)),
            ("url", MetaString info.deckUrl),
            ("id", maybe (MetaBool False) (MetaString . toText) info.deckId),
            ("title", maybe (MetaBool False) (MetaString . toText) info.deckTitle),
            ("subtitle", maybe (MetaBool False) (MetaString . toText) info.deckSubtitle)
          ]
    toQuestMeta :: QuestInfo -> MetaValue
    toQuestMeta info =
      MetaMap
        $ fromList
          [ ("src", MetaString (toText info.questSrc)),
            ("url", MetaString info.questUrl),
            ("title", MetaString info.questTitle),
            ("lecture-id", MetaString info.questLectureId),
            ("topic-id", MetaString info.questTopicId)
          ]
    sortInfo by = sortBy (\a b -> compare (by a) (by b))
