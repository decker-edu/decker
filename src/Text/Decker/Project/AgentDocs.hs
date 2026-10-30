{-# LANGUAGE NoImplicitPrelude #-}

-- | Support for AI agents that author Decker content.
--
--   * @decker agent-guide [TOPIC]@ prints the agent guide. It is split into
--     topics; without a topic the index is printed, with @--all@ every topic.
--     The guide is produced by the running binary, so it always matches its
--     version, and it is never copied into a project.
--
--   * @decker agent-docs@ installs small pointer files that tell an agent to
--     run @decker agent-guide@: the Claude skill
--     @.claude/skills/decker/SKILL.md@ and a delimited section in @AGENTS.md@.
--     Their content does not depend on the Decker version, so re-running after
--     an upgrade changes nothing and collaborators on different versions do
--     not fight over them.
--
-- The topics live in @template/agent-guide/<topic>.md@ in the resources. A
-- resource pack adds topics by shipping its own files there; a pack topic
-- replaces a Decker topic with the same name. The pointer files are rendered
-- from @template/skill.md@ and @template/agents-md.md@, which a pack can
-- override as well. All of these are plain Markdown, not doctemplates.
module Text.Decker.Project.AgentDocs
  ( AgentDocsOpts (..),
    defaultAgentDocsOpts,
    runAgentDocs,
    runAgentGuide,
    guideTopicNames,
    guideTopicDir,
    replaceSection,
  )
where

import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (dropExtension, takeDirectory, takeExtension, (<.>), (</>))
import Text.Decker.Project.Version (deckerGitBranch, deckerVersion, isDevelopmentVersion)
import Text.Decker.Resource.Resource (Resources (..), deckerResources, fileList, readResource')
import Text.Pandoc (Meta)

-- | The resource directory holding the guide topics.
guideTopicDir :: FilePath
guideTopicDir = "template" </> "agent-guide"

-- | The topic printed by @decker agent-guide@ without arguments.
indexTopic :: Text
indexTopic = "index"

-- | Replaced in the index topic by the list of available topics.
topicListMarker :: Text
topicListMarker = "<!-- decker:topics -->"

-- | E.g. @0.15.0 (dev, branch develop)@.
versionStamp :: Text
versionStamp =
  toText deckerVersion
    <> " ("
    <> (if isDevelopmentVersion then "dev" else "release")
    <> ", branch "
    <> toText deckerGitBranch
    <> ")"

-- | The names of all topics except the index, from Decker and the resource
-- pack, sorted.
guideTopicNames :: Meta -> IO [Text]
guideTopicNames meta = do
  Resources decker pack <- deckerResources meta
  files <- concat <$> mapM (`fileList` guideTopicDir) [pack, decker]
  return $
    List.sort $
      ordNub
        [ toText (dropExtension file)
          | file <- files,
            takeExtension file == ".md",
            toText (dropExtension file) /= indexTopic
        ]

readTopic :: Meta -> Text -> IO (Maybe Text)
readTopic meta topic =
  fmap decodeUtf8 <$> readResource' (guideTopicDir </> toString topic <.> "md") meta

-- | The topic's title: its first level-1 heading.
topicTitle :: Text -> Text
topicTitle content =
  fromMaybe "" $ listToMaybe [Text.strip title | line <- lines content, Just title <- [Text.stripPrefix "# " line]]

-- | Prints the index (no topic), the given topics, or with @all@ everything.
-- Fails with the list of topics if a topic does not exist.
runAgentGuide :: Meta -> Bool -> [String] -> IO ()
runAgentGuide meta everything requested = do
  topics <- guideTopicNames meta
  let selected
        | everything = indexTopic : topics
        | null requested = [indexTopic]
        | otherwise = map toText requested
  contents <- forM selected $ \topic -> (topic,) <$> readTopic meta topic
  case [topic | (topic, Nothing) <- contents] of
    [] -> do
      rendered <- forM [(t, c) | (t, Just c) <- contents] $ \(topic, content) ->
        if topic == indexTopic
          then renderIndex topics content
          else return content
      putTextLn $ "<!-- decker agent-guide " <> unwords selected <> " · decker " <> versionStamp <> " -->\n"
      putText $ Text.intercalate "\n---\n\n" rendered
    missing -> do
      Text.hPutStrLn stderr $
        "Unknown agent guide topic: "
          <> unwords missing
          <> "\nAvailable topics: "
          <> unwords topics
      exitFailure
  where
    renderIndex topics content = do
      entries <- forM topics $ \topic -> do
        title <- maybe "" topicTitle <$> readTopic meta topic
        return $ "- `decker agent-guide " <> topic <> "` — " <> title
      return $ Text.replace topicListMarker (unlines entries) content

data AgentDocsOpts = AgentDocsOpts
  { -- | Write the Claude skill (default True; @--no-skill@ to skip).
    adoSkill :: Bool,
    -- | Write the section in AGENTS.md (default True; @--no-agents-md@ to
    -- skip).
    adoAgentsMd :: Bool
  }

defaultAgentDocsOpts :: AgentDocsOpts
defaultAgentDocsOpts = AgentDocsOpts {adoSkill = True, adoAgentsMd = True}

-- | The committed skill path. Fixed so clones auto-discover it.
skillPath :: FilePath
skillPath = ".claude" </> "skills" </> "decker" </> "SKILL.md"

agentsMdPath :: FilePath
agentsMdPath = "AGENTS.md"

-- | Where earlier versions wrote a full, version-stamped copy of the guide.
staleGuidePath :: FilePath
staleGuidePath = ".decker" </> "agent-guide.md"

sectionBegin, sectionEnd :: Text
sectionBegin = "<!-- decker:begin -->"
sectionEnd = "<!-- decker:end -->"

-- | Writes the pointer files. Paths are relative to the current directory,
-- which decker has already set to the project root.
runAgentDocs :: Meta -> AgentDocsOpts -> IO ()
runAgentDocs meta opts = do
  when (adoSkill opts) $ do
    skill <- readPointerTemplate "template/skill.md"
    writeArtifact skillPath skill
  when (adoAgentsMd opts) $ do
    section <- readPointerTemplate "template/agents-md.md"
    exists <- doesFileExist agentsMdPath
    current <- if exists then Text.readFile agentsMdPath else return ""
    writeArtifact agentsMdPath (replaceSection section current)
  whenM (doesFileExist staleGuidePath) $ do
    removeFile staleGuidePath
    putStrLn $ "Removed " <> staleGuidePath <> " (replaced by `decker agent-guide`)"
  where
    readPointerTemplate path =
      readResource' path meta
        >>= maybe (fail $ "Missing resource: " <> path) (return . decodeUtf8)

-- | Replaces the delimited Decker section in a file's content, or appends it if
-- there is none. Everything outside the section is left alone.
replaceSection :: Text -> Text -> Text
replaceSection section current =
  case Text.breakOn sectionBegin current of
    (before, rest)
      | not (Text.null rest),
        (_, after) <- Text.breakOn sectionEnd rest,
        not (Text.null after) ->
          before <> delimited <> Text.drop (Text.length sectionEnd) after
    _
      | Text.null (Text.strip current) -> delimited <> "\n"
      | otherwise -> Text.stripEnd current <> "\n\n" <> delimited <> "\n"
  where
    delimited = sectionBegin <> "\n" <> Text.strip section <> "\n" <> sectionEnd

-- | Write @contents@ to @path@, creating parent directories. Idempotent: if the
-- file already holds exactly these contents, nothing is written, so re-running
-- keeps the git tree clean.
writeArtifact :: FilePath -> Text -> IO ()
writeArtifact path contents = do
  exists <- doesFileExist path
  current <- if exists then Just <$> Text.readFile path else pure Nothing
  if current == Just contents
    then putStrLn $ "Up to date " <> path
    else do
      createDirectoryIfMissing True (takeDirectory path)
      Text.writeFile path contents
      putStrLn $ "Wrote " <> path
