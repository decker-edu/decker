{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The @decker agent-docs@ command. Emits two version-stamped artifacts that
-- teach an AI agent how to author and validate Decker content:
--
--   * @.decker/agent-guide.md@ — the full, gitignored, regenerable guide.
--   * @.claude/skills/decker/SKILL.md@ — the committed, durable entry point that
--     travels with the repo so any clone auto-discovers it.
--
-- Both are produced from templates that live alongside the other default
-- templates in @resource/decker/template@ (@agent-guide.md@ and @skill.md@), so a
-- resource pack can override them like any other template. They are resolved
-- through 'readTemplate' and rendered with the 'Text.DocTemplates' substitution
-- mechanism: the @$decker-version-stamp$@ variable carries the running binary's
-- version, so the artifacts cannot silently go stale.
module Text.Decker.Project.AgentDocs
  ( AgentDocsOpts (..),
    defaultAgentDocsOpts,
    runAgentDocs,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import Text.Decker.Project.Version (deckerGitBranch, deckerVersion, isDevelopmentVersion)
import Text.Decker.Resource.Template (readTemplate)
import Text.DocLayout (render)
import Text.DocTemplates
  ( Context (Context),
    Doc (Text),
    Template,
    Val (SimpleVal),
    renderTemplate,
  )
import Text.Pandoc (Meta)

-- | The full guide template, overridable via a resource pack.
guideTemplatePath :: FilePath
guideTemplatePath = "template/agent-guide.md"

-- | The Claude skill template, overridable via a resource pack.
skillTemplatePath :: FilePath
skillTemplatePath = "template/skill.md"

-- | The version stamp substituted for the @$decker-version-stamp$@ template
-- variable, e.g. @0.15.0 (dev, branch develop)@.
versionStamp :: Text
versionStamp =
  Text.pack deckerVersion
    <> " ("
    <> (if isDevelopmentVersion then "dev" else "release")
    <> ", branch "
    <> Text.pack deckerGitBranch
    <> ")"

-- | Resolve a template through the resource system (pack overrides default) and
-- render it with the version stamp bound to @$decker-version-stamp$@.
renderTemplateFile :: Meta -> FilePath -> IO Text
renderTemplateFile meta file = do
  (template, _needed) <- readTemplate meta file
  return $ renderStamped template

-- | Render a compiled template, substituting the version stamp.
renderStamped :: Template Text -> Text
renderStamped template =
  render Nothing (renderTemplate template context)
  where
    context =
      Context $
        Map.fromList
          [("decker-version-stamp", SimpleVal (Text 0 versionStamp))]

data AgentDocsOpts = AgentDocsOpts
  { -- | Print the rendered guide to stdout and write nothing.
    adoStdout :: Bool,
    -- | Override the guide output path (guide only).
    adoOutput :: Maybe FilePath,
    -- | Write the committed Claude skill (default True; @--no-skill@ to skip).
    adoSkill :: Bool,
    -- | Write the gitignored agent guide (default True; @--no-guide@ to skip).
    adoGuide :: Bool
  }

-- | Defaults: write both artifacts to their default paths.
defaultAgentDocsOpts :: AgentDocsOpts
defaultAgentDocsOpts =
  AgentDocsOpts
    { adoStdout = False,
      adoOutput = Nothing,
      adoSkill = True,
      adoGuide = True
    }

-- | The default (gitignored, regenerable) guide path.
defaultGuidePath :: FilePath
defaultGuidePath = ".decker" </> "agent-guide.md"

-- | The committed skill path. Fixed so clones auto-discover it.
skillPath :: FilePath
skillPath = ".claude" </> "skills" </> "decker" </> "SKILL.md"

-- | Emit the requested artifacts. Paths are relative to the current directory,
-- which decker has already set to the project root. The 'Meta' selects the
-- resource pack (if any) that may override the templates.
runAgentDocs :: Meta -> AgentDocsOpts -> IO ()
runAgentDocs meta opts
  | adoStdout opts = renderTemplateFile meta guideTemplatePath >>= putText
  | otherwise = do
      when (adoGuide opts) $ do
        guide <- renderTemplateFile meta guideTemplatePath
        writeArtifact (fromMaybe defaultGuidePath (adoOutput opts)) guide
      when (adoSkill opts) $ do
        skill <- renderTemplateFile meta skillTemplatePath
        writeArtifact skillPath skill

-- | Write @contents@ to @path@, creating parent directories. Idempotent: if the
-- file already holds exactly these contents (same version stamp), nothing is
-- written, so re-running keeps the git tree clean.
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
