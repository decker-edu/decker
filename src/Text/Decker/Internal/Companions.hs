-- | The one definition of companion files: the runtime data a deck loads from
-- JavaScript (annotations, timings, recordings, captions, transcripts). They
-- live next to the deck source (@foo-deck.md@ → @foo-annot.json@). They are
-- not build inputs. A build only copies them to @public/@. The server, the
-- project scanner and the file watcher all use the lists here, so they cannot
-- drift apart.
module Text.Decker.Internal.Companions
  ( Suffix (..),
    publishedSuffixes,
    uploadableSuffixes,
    matchesSuffix,
    isPublishedCompanion,
    isUploadable,
    isCompanion,
    isRecordingArtifact,
    deckBase,
    isCompanionOf,
  )
where

import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import System.FilePath.Posix (splitFileName, takeFileName)

-- | A file name suffix that follows the deck base name (the deck source path
-- without @-deck.md@).
data Suffix
  = -- | Exactly this string, e.g. @-annot.json@.
    Exact String
  | -- | Starts with the first and ends with the second string, with a
    -- non-empty part in between, e.g. @-recording-*.vtt@.
    Pattern String String
  deriving (Eq, Show)

-- | Companions that a build copies to @public/@.
publishedSuffixes :: [Suffix]
publishedSuffixes =
  [ Exact "-annot.json",
    Exact "-manip.json",
    Exact "-times.json",
    Exact "-transcript.json",
    Exact "-recording.mp4",
    Exact "-recording.vtt",
    Pattern "-recording-" ".vtt"
  ]

-- | Companions that the browser may upload to the dev server: the JSON and
-- fixed-name VTT entries of 'publishedSuffixes' plus @-poll.json@, which is
-- intentionally never published. Videos have their own upload path, and the
-- @-recording-*.vtt@ captions are produced by @decker transcribe@.
uploadableSuffixes :: [Suffix]
uploadableSuffixes =
  [ Exact "-annot.json",
    Exact "-manip.json",
    Exact "-times.json",
    Exact "-transcript.json",
    Exact "-recording.vtt",
    Exact "-poll.json"
  ]

-- | Does the base name @base@ followed by the suffix give the file name @name@?
suffixFollows :: String -> String -> Suffix -> Bool
suffixFollows base name (Exact s) = name == base <> s
suffixFollows base name (Pattern pre post) =
  case stripPrefix (base <> pre) name of
    Just rest -> post `isSuffixOf` rest && length rest > length post
    Nothing -> False

-- | Does the file name end on the suffix, with a non-empty base name before
-- it?
matchesSuffix :: Suffix -> FilePath -> Bool
matchesSuffix suffix path =
  any (\n -> suffixFollows (take n name) name suffix) [1 .. length name - 1]
  where
    name = takeFileName path

isPublishedCompanion :: FilePath -> Bool
isPublishedCompanion path = any (`matchesSuffix` path) publishedSuffixes

isUploadable :: FilePath -> Bool
isUploadable path = any (`matchesSuffix` path) uploadableSuffixes

-- | Published or uploadable companion.
isCompanion :: FilePath -> Bool
isCompanion path = isPublishedCompanion path || isUploadable path

-- | Video files and the parts of a running recording session:
-- @*-recording*.webm@, @*-recording.mp4@, @*.part@, @*.part.json@.
isRecordingArtifact :: FilePath -> Bool
isRecordingArtifact path =
  (".webm" `isSuffixOf` name && "-recording" `isInfixOf` drop 1 name)
    || matchesSuffix (Exact "-recording.mp4") name
    || any (`isSuffixOf` name) [".part", ".part.json"]
  where
    name = takeFileName path

-- | The deck base name of a deck source path: @dir/foo-deck.md@ → @dir/foo@.
deckBase :: FilePath -> Maybe FilePath
deckBase source = do
  let (dir, name) = splitFileName source
  base <- stripSuffix "-deck.md" name
  if null base then Nothing else Just (dir <> base)
  where
    stripSuffix s str = reverse <$> stripPrefix (reverse s) (reverse str)

-- | Is the path a published companion of the deck source? Both paths must be
-- given relative to the same directory.
isCompanionOf :: FilePath -> FilePath -> Bool
isCompanionOf source path = case deckBase source of
  Nothing -> False
  Just base ->
    let (baseDir, baseName) = splitFileName base
        (dir, name) = splitFileName path
     in dir == baseDir && any (suffixFollows baseName name) publishedSuffixes

