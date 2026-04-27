{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.FragmentTemplate (expandFragmentTemplates) where

import Control.Concurrent.STM (modifyTVar)
import Control.Exception (throw)
import System.Directory (doesFileExist)
import Data.Aeson qualified as A
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Development.Shake.FilePath ((</>))
import Relude
import System.FilePath ((<.>))
import Text.Decker.Filter.Monad (Filter, FilterState (templates), meta)
import Text.Decker.Filter.Util (randomId)
import Text.Decker.Filter.Local 
import Text.Decker.Internal.Common (projectDir)
import Text.Decker.Internal.Exception (DeckerException (..))
import Text.Decker.Internal.Meta (fromPandocMeta, lookupMetaOrElse)
import Text.Decker.Internal.URI (makeProjectPath)
import Text.Decker.Resource.Resource
import Text.DocLayout (render)
import Text.DocTemplates (Context, compileTemplateFile, compileTemplate, toContext)
import Text.Pandoc
  ( Block (CodeBlock, Para, Plain, RawBlock),
    Inline (Link, RawInline),
    Pandoc (..),
    Template,
    renderTemplate,
  )
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (Walkable (walkM))
import Text.Regex.TDFA.Text ()

expandFragmentTemplates :: Pandoc -> Filter Pandoc
expandFragmentTemplates document@(Pandoc meta blocks) =
  if lookupMetaOrElse False "experiments.fragment-templates" meta
    then Pandoc meta <$> (walkM expandBlockM blocks >>= walkM expandLinkM)
    else return document
  where
    -- Expands macro links in block contexts
    expandBlockM :: Block -> Filter Block
    expandBlockM (Para [link]) = do
      link <- expandLinkM link
      return (Para [link])
    expandBlockM (Plain [link]) = do
      link <- expandLinkM link
      return (Plain [link])
    expandBlockM block@(CodeBlock attr code) = expandCodeM block
    expandBlockM block = return block

    -- Expands macro links in inline contexts
    -- expandLinkM :: Inline -> Filter a
    expandLinkM link@(Link attr@(id, cls, kvs) inlines (url, title)) = do
      let rawKvs = map (\(k, v) -> (fromMaybe k $ Text.stripPrefix "data-" k, v)) kvs
      case parseInvocation inlines of
        Just (name, args) -> do
          let kvAttribs = List.filter ((/= "micro") . fst) rawKvs
          let targetArgs = [("url", url), ("title", title)]
          let posArgs = zip (map (("arg" <>) . show) [1 .. (length args)]) args
          let allPosArgs = [("args", Text.unwords args)]
          let argCount = [("argn", show $ length args)]
          let clsArgs = zip (map (("class" <>) . show) [1 .. (length cls)]) cls
          let allClsArgs = [("classes", Text.unwords cls)]
          let allKvAttribs = [("attribs", unwords $ map (\(k, v) -> k <> "=\"" <> v <> "\"") kvAttribs)]
          rndId <- liftIO randomId
          let idArg = [("id", if Text.null id then rndId else id)]
          let arguments = allPosArgs <> posArgs <> targetArgs <> idArg <> clsArgs <> allClsArgs <> kvAttribs <> allKvAttribs <> argCount
          let metaData = fromPandocMeta meta
          let json = map (second A.String) arguments
          let all = json <> [("meta", metaData)]
          template <- getTemplate (toString name)
          let context :: Context Text = toContext $ Map.fromList all
          let text :: Text = render Nothing $ renderTemplate template context
          return $ RawInline "html" text
        Nothing ->
          return link
    expandLinkM link = return link

    expandCodeM block@(CodeBlock attr@(id, cls, kvs) code) = do
      let rawKvs = map (\(k, v) -> (fromMaybe k $ Text.stripPrefix "data-" k, v)) kvs
      let name = listToMaybe $ catMaybes [List.lookup "micro" rawKvs, Text.dropEnd 1 <$> find (Text.isSuffixOf "-") cls]
      case name of
        Just name -> do
          rndId <- ("id" <>) <$> liftIO randomId
          let kvAttribs = List.filter ((/= "micro") . fst) rawKvs
          let clsArgs = zip (map (("class" <>) . show) [1 .. (length cls)]) cls
          let allClsArgs = [("classes", Text.unwords cls)]
          let allKvAttribs = [("attribs", unwords $ map (\(k, v) -> k <> "=\"" <> v <> "\"") kvAttribs)]
          let codeArg = [("code", Text.strip code)]
          let codeEscArg = [("codeEsc", escape $ Text.strip code)]
          let rndIdArg = [("rnd-id", rndId)]
          let idArg = [("id", if Text.null id then rndId else id)]
          let captionArg = [("caption", fromMaybe "" (List.lookup "caption" rawKvs))]
          let arguments :: [(Text, Text)] = codeArg <> codeEscArg <> clsArgs <> allClsArgs <> kvAttribs <> allKvAttribs <> rndIdArg <> idArg <> captionArg
          let metaData = fromPandocMeta meta
          let json = map (second A.String) arguments
          let all = json <> [("meta", metaData)]
          template <- getTemplate (toString name)
          let context :: Context Text = toContext $ Map.fromList all
          let text :: Text = render Nothing $ renderTemplate template context
          return $ RawBlock "html" text
        Nothing ->
          return block
    expandCodeM block = return block
    escape = Text.replace "<" "&lt;" . Text.replace "<" "&lt;"

    -- Parses a link text into a macro invocation, if possible. a macro name
    -- starts either with  a '§', or ends with a '-'
    parseInvocation inline =
      case second Text.words $ Text.splitAt 1 $ stringify inline of
        ("§", name : args) -> Just (name, args)
        _ -> case Text.words $ stringify inline of
          (name : args) | Text.isSuffixOf "-" name -> Just (Text.dropEnd 1 name, args)
          _ -> Nothing

getTemplate :: String -> Filter (Template Text)
getTemplate filename = do
  meta <- gets meta
  tvar <- gets templates
  templates <- liftIO $ readTVarIO tvar
  case Map.lookup filename templates of
    Nothing -> do
      template <- do
        readTemplateFile (filename <.> "html")
      atomically $ modifyTVar tvar (Map.insert filename template)
      return template
    Just template -> return template

-- | Try to compile a template file, distinguishing between "file not found"
-- (returns Nothing) and "compile error" (returns Just (Left error)).
tryCompileTemplateFile :: FilePath -> IO (Maybe (Either String (Template Text)))
tryCompileTemplateFile path = do
  exists <- doesFileExist path
  if exists
    then Just <$> compileTemplateFile path
    else return Nothing

readTemplateFile :: String -> Filter (Template Text)
readTemplateFile filename = do
  meta <- gets meta
  let base :: String = lookupMetaOrElse "." "decker.base-dir" meta
  let path1 = makeProjectPath base filename
  let path2 = projectDir </> "templates" </> filename
  let searchPaths = [path1, path2]

  -- Try each file path in order. Stop on the first that exists.
  result <- liftIO $ firstJustM tryCompileTemplateFile searchPaths
  case result of
    Just (Right template) -> do
      return template
    Just (Left err) ->
      throw (ResourceException $ "Cannot compile template '" <> filename <> "': " <> err)
    Nothing -> do
      -- File not found in project paths, try reading as a bundled resource
      let resourcePath = "support/templates" </> filename
      text <- fmap decodeUtf8 <$> liftIO (readResource' resourcePath meta)
      case text of
        Just text -> do
          result <- liftIO $ Text.DocTemplates.compileTemplate filename text
          case result of
            Right template -> return template
            Left err ->
              throw (ResourceException $ "Cannot compile resource template '" <> filename <> "': " <> err)
        Nothing ->
          throw (ResourceException $ "Cannot find template '" <> filename <> "'. Searched:\n"
            <> List.intercalate "\n" (map ("  - " <>) searchPaths)
            <> "\n  - resource:" <> resourcePath)

firstJustM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = return Nothing
firstJustM f (x : xs) = do
  result <- f x
  case result of
    Just v -> return (Just v)
    Nothing -> firstJustM f xs

