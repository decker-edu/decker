{-# LANGUAGE NoImplicitPrelude #-}

-- | @decker info --json@: project facts for tools and AI agents, printed as one
-- JSON document on stdout. Runs in plain IO, without Shake.
--
-- The @config@ object is a curated subset of the global meta data. The meta
-- data can hold chatty and publishing settings that do not belong in an
-- agent's context, so it is never dumped as a whole.
module Text.Decker.Project.Info (projectInfo, printProjectInfo) where

import Control.Lens ((^.))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty', keyOrder)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Relude
import System.Directory qualified as Dir
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta (lookupMeta, lookupMetaOrElse)
import Text.Decker.Project.AgentDocs (guideTopicNames)
import Text.Decker.Project.Glob (fastGlobFiles)
import Text.Decker.Project.Project
import Text.Decker.Project.Version (deckerGitBranch, deckerVersion, isDevelopmentVersion)
import Text.Decker.Resource.Resource
import Text.Pandoc (Meta)

-- | Version of the JSON layout. Increment on incompatible changes.
schemaVersion :: Int
schemaVersion = 1

printProjectInfo :: Meta -> IO ()
printProjectInfo meta = do
  info <- projectInfo meta
  LBS.putStrLn $ encodePretty' config info
  where
    config =
      defConfig
        { confCompare =
            keyOrder
              [ "schema",
                "decker",
                "directories",
                "resourcePack",
                "config",
                "targets",
                "aggregates",
                "fragmentTemplates",
                "guideTopics",
                "source",
                "kind",
                "outputs",
                "name",
                "origin",
                "path"
              ]
        }

-- | Collects the project facts. Paths are relative to the project directory,
-- which is the current directory.
projectInfo :: Meta -> IO Value
projectInfo meta = do
  project <- Dir.makeAbsolute projectDir
  Resources decker pack <- deckerResources meta
  targets <- scanTargets meta
  hasIndex <- Dir.doesFileExist indexSource
  templates <- fragmentTemplates decker pack
  topics <- guideTopicNames meta
  return $
    object
      [ "schema" .= schemaVersion,
        "decker"
          .= object
            [ "version" .= deckerVersion,
              "branch" .= deckerGitBranch,
              "development" .= isDevelopmentVersion
            ],
        "directories"
          .= object
            [ "project" .= project,
              "public" .= publicDir,
              "private" .= privateDir,
              "support" .= supportDir
            ],
        "resourcePack"
          .= object
            [ "configured" .= (lookupMeta "resource-pack" meta :: Maybe Text),
              "resolved" .= sourceText pack
            ],
        "config" .= curatedConfig meta,
        "targets" .= targetList hasIndex targets,
        "aggregates"
          .= object
            [ "examCatalog" .= (publicDir </> "exam-builder" </> "questions.json"),
              "questionCatalog" .= (privateDir </> "quest-catalog.html")
            ],
        "fragmentTemplates" .= templates,
        "guideTopics" .= topics
      ]

curatedConfig :: Meta -> Value
curatedConfig meta =
  object
    [ "lang" .= text "lang",
      "bibliography" .= text "bibliography",
      "csl" .= text "csl",
      "strict" .= (lookupMetaOrElse False "strict" meta :: Bool),
      -- Decks scale this box to the window; screenshots should match it.
      "slideSize"
        .= object
          [ "width" .= number "reveal.width",
            "height" .= number "reveal.height"
          ],
      "experiments" .= experiments,
      -- Packs switch these off, and a feature that needs one then fails silently.
      "deckerPlugins" .= switches "decker-plugins",
      "pageFeatures" .= switches "page-features",
      "codapiServer" .= text "codapi.server",
      "excludeDirectories" .= (lookupMetaOrElse [] "exclude-directories" meta :: [Text]),
      "staticResourceDirs"
        .= ( lookupMetaOrElse [] "static-resource-dirs" meta
               <> lookupMetaOrElse [] "static-resources" meta ::
               [Text]
           )
    ]
  where
    text key = lookupMeta key meta :: Maybe Text
    -- YAML numbers are stored as strings like "1280.0".
    number key = round <$> (lookupMeta key meta :: Maybe Float) :: Maybe Int
    switches key = lookupMetaOrElse Map.empty key meta :: Map.Map Text Bool
    -- Only boolean switches; the experiments map holds nothing else.
    experiments =
      object
        [ (fromString (toString key), Bool on)
          | key <- ["fragment-templates", "add-document-path"],
            Just on <- [lookupMeta ("experiments." <> key) meta]
        ]

-- | One entry per source, with all the outputs Decker builds from it.
targetList :: Bool -> Targets -> [Value]
targetList hasIndex targets =
  map entry $ Map.toList $ Map.fromListWith merge $ index <> concatMap outputs kinds
  where
    merge (kind, new) (_, old) = (kind, old <> new)
    kinds =
      [ ("deck", targets ^. decks),
        ("deck", targets ^. handouts),
        ("page", targets ^. pages),
        ("quest", targets ^. questions),
        ("exam", targets ^. exams)
      ]
    outputs (kind, deps) = [(source, (kind, [target])) | (target, source) <- Map.toList deps]
    index = [(indexSource, ("index" :: Text, [publicDir </> "index.html"])) | hasIndex]
    entry (source, (kind, outs)) =
      object ["source" .= source, "kind" .= kind, "outputs" .= List.sort outs]

-- | The fragment templates a document can use, each with the origin that wins
-- the lookup (project before resource pack before Decker). Templates relative
-- to @decker.base-dir@ are not listed; they depend on the document.
fragmentTemplates :: Source -> Source -> IO [Value]
fragmentTemplates decker pack = do
  exists <- Dir.doesDirectoryExist "templates"
  project <- if exists then fastGlobFiles [] [".html"] "templates" else return []
  fromPack <- names <$> fileList pack "support/templates"
  fromDecker <- names <$> fileList decker "support/templates"
  let projectNames = names project
      byName =
        Map.unions
          [ Map.fromList [(n, ("project" :: Text, Just path)) | (n, path) <- zip projectNames project],
            Map.fromList [(n, ("pack", Nothing)) | n <- fromPack],
            Map.fromList [(n, ("decker", Nothing)) | n <- fromDecker]
          ]
  return $ map entry (Map.toList byName)
  where
    names = map (dropExtension . takeFileName) . filter ((== ".html") . takeExtension)
    entry (name, (origin, path)) =
      object $ ["name" .= name, "origin" .= origin] <> maybe [] (\p -> ["path" .= p]) path

-- | A resource source as a URI-like string.
sourceText :: Source -> Maybe Text
sourceText (DeckerExecutable path) = Just $ "exe:" <> toText path
sourceText (LocalDir path) = Just $ toText path
sourceText (LocalZip path) = Just $ toText path
sourceText None = Nothing
