{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Writer.Layout (markdownToHtml, writePandocFile, writeHtml45String) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BS
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Development.Shake
import Relude
import Skylighting (SyntaxMap, defaultSyntaxMap, loadSyntaxFromFile)
import System.FilePath
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (ChoiceString (..), MarkupM (..), StaticString, getString, getText)
import Text.Decker.Filter.Util (hash9String)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.DocTemplates
  ( Context (Context),
    Doc (Text),
    Val (SimpleVal),
  )
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Highlighting
import Prelude qualified

-- | Calculates the highlight style that will be passed as a writer option.
highlightStyle :: Meta -> Maybe Style
highlightStyle meta =
  let pandocStyle = lookupMeta "highlight-style" meta :: Maybe Text
      hljsStyle = lookupMeta "highlightjs" meta :: Maybe Text
   in case hljsStyle of
        Just _ -> Nothing
        Nothing -> case pandocStyle of
          Just ps -> lookup ps highlightingStyles
          Nothing -> lookup "monochrome" highlightingStyles

-- | Calculates the syntax map for the writer option. Loads additoional syntax
-- files from the meta-data list `extra-highlight-syntax`.
getHighlightSyntax :: Meta -> Action SyntaxMap
getHighlightSyntax meta = do
  let additionalSyntax =
        maybe [] Map.toList $ lookupMeta "extra-highlight-syntax" meta
  extras <- Map.fromList . catMaybes <$> mapM loadSyntax additionalSyntax
  return $ Map.union defaultSyntaxMap extras
  where
    loadSyntax (key :: Text, path :: FilePath) = do
      result <- liftIO $ loadSyntaxFromFile path
      case result of
        Left err -> do
          putError
            $ "# cannot load highlighting syntax for: "
            <> toString key
            <> " from file: "
            <> path
          return Nothing
        Right syntax -> return $ Just (key, syntax)

markdownToHtml :: Disposition -> Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtml disp meta getTemplate markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  pandoc@(Pandoc meta _) <- readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  syntaxMap <- getHighlightSyntax meta
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerSectionDivs = False,
            writerHighlightStyle = highlightStyle meta,
            writerSyntaxMap = syntaxMap,
            writerHTMLMathMethod =
              MathJax (lookupMetaOrElse "" "mathjax-url" meta),
            writerVariables =
              Context
                $ fromList
                  [ ( "decker-support-dir",
                      SimpleVal $ Text.DocTemplates.Text 0 $ toText relSupportDir
                    )
                  ],
            writerCiteMethod = Citeproc
          }
  writePandocFile options out pandoc

-- | writes a document in two steps. First the document is written as a fragment
-- of plain HTML 4. which is then adjusted for reveal compatible section tags.
-- Finally, the fragment is inserted into a Reveal.js slide deck template.
writePandocFile :: WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile options out pandoc@(Pandoc meta blocks) = do
  let metaFile = hash9String out <.> ".json"
  let metaPath = takeDirectory out </> metaFile
  let meta' = addMetaKeyValue "decker-meta-url" (toText metaFile) meta
  let metaJson = encodePretty $ fromPandocMeta meta'
  liftIO $ BS.writeFile metaPath metaJson
  -- liftIO $ do
  --   html <-
  --     runIO (setVerbosity ERROR >> writeHtml4 options {writerTemplate = Nothing} pandoc) >>= handleError
  --   let string = renderHtml $ transformHtml (nullA, []) html
  --   let raw =
  --         [ RawBlock "html" $ fromLazy string,
  --           Plain
  --             [ Code
  --                 ( "",
  --                   ["force-highlight-styles", "markdown"],
  --                   [("style", "display:none;")]
  --                 )
  --                 ""
  --             ]
  --         ]
  -- runIO (writeHtml5String options (embedMetaMeta (Pandoc meta raw)))
  liftIO
    $ runIO (setVerbosity ERROR >> writeHtml45String options meta' pandoc)
    >>= handleError
    >>= Text.writeFile out

writeHtml45String :: (PandocMonad m) => WriterOptions -> Meta -> Pandoc -> m Text
writeHtml45String options meta pandoc = do
  html <- writeHtml4 options {writerTemplate = Nothing} pandoc
  let string = renderHtml $ transformHtml (nullA, []) html
  let raw =
        [ RawBlock "html" $ fromLazy string,
          Plain
            [ Code
                ( "",
                  ["force-highlight-styles", "markdown"],
                  [("style", "display:none;")]
                )
                ""
            ]
        ]
  writeHtml5String options (Pandoc meta raw)

-- | Sets or resets the "incremental" flag.
incremental :: Bool -> [Text] -> [Text]
incremental True flags = "incremental" : filter (/= "nonincremental") flags
incremental False flags = "nonincremental" : filter (/= "incremental") flags

-- | Transforms a HTML structure such that divs with a attribute
-- data-tag=section are transformed into section elements with the data-tag
-- attribute removed.
--
-- Also, the class "processed" is removed from all elements.
transformHtml :: (Map Text Text, [Text]) -> MarkupM a -> MarkupM a
transformHtml (attribs, flags) m@(Parent tag open end html)
  | getText tag `elem` ["div", "span"] && Map.member "data-tag" attribs =
      let name = toString $ attribs Map.! "data-tag"
          flags' = incremental (hasClass "incremental" attribs) flags
       in Parent
            (fromString name)
            (fromString $ "<" <> name)
            (fromString $ "</" <> name <> ">")
            (transformHtml (nullA, flags') html)
transformHtml (attribs, flags) m@(Parent tag open end html)
  | getText tag `elem` ["div"] && hasClass "incremental" attribs =
      Parent tag open end (transformHtml (nullA, incremental True flags) html)
transformHtml (attribs, flags) m@(Parent tag open end html)
  | getText tag `elem` ["div"] && hasClass "nonincremental" attribs =
      Parent tag open end (transformHtml (nullA, incremental False flags) html)
-- Tags all <li> below <div class="incremental"> as `fragment`
transformHtml (attribs, flags) m@(Parent tag open end html)
  | getText tag == "li" && "incremental" `elem` flags =
      AddCustomAttribute "class" "fragment" $ Parent tag open end (transformHtml (nullA, flags) html)
-- Discards the collected attributes and recurse
transformHtml (attribs, flags) m@(Parent tag open end html) =
  Parent tag open end (transformHtml (nullA, flags) html)
-- Discards the collected attributes and recurse
transformHtml (attribs, flags) m@(CustomParent string html) =
  CustomParent string (transformHtml (nullA, flags) html)
-- Discards the collected attributes and recurse twice
transformHtml (attribs, flags) m@(Append html1 html2) =
  Append (transformHtml (nullA, flags) html1) (transformHtml (nullA, flags) html2)
-- Drops 'processed' from class attribute
transformHtml (attribs, flags) m@(AddAttribute raw key value html)
  | toText raw == "class" =
      let cls = Text.unwords $ filter (/= "processed") $ Text.words $ toText value
       in if Text.null cls
            then transformHtml (attribs, flags) html
            else AddAttribute raw key value (transformHtml (Map.insert (toText raw) cls attribs, flags) html)
-- Adds the attribute to the map for later retrieval
transformHtml (attribs, flags) m@(AddAttribute raw key value html) =
  AddAttribute raw key value (transformHtml (Map.insert (toText raw) (toText value) attribs, flags) html)
-- Drops the custom data-tag attribute after adding it to the map
transformHtml (attribs, flags) m@(AddCustomAttribute key value html)
  | toText key == "data-tag" =
      transformHtml (Map.insert (toText key) (toText value) attribs, flags) html
-- Drops 'processed' from class attribute
transformHtml (attribs, flags) m@(AddCustomAttribute key value html)
  | toText key == "class" =
      let cls = Text.unwords $ filter (/= "processed") $ Text.words $ toText value
       in if Text.null cls
            then transformHtml (attribs, flags) html
            else AddCustomAttribute key value (transformHtml (Map.insert (toText key) cls attribs, flags) html)
-- Adds the custom attribute to the map for later retrieval
transformHtml (attribs, flags) m@(AddCustomAttribute key value html) =
  AddCustomAttribute key value (transformHtml (Map.insert (toText key) (toText value) attribs, flags) html)
transformHtml (attribs, flags) m = m

hasClass :: Text -> Map Text Text -> Bool
hasClass cls attrib =
  case Map.lookup "class" attrib of
    Nothing -> False
    Just txt -> (cls `elem`) $ Text.words txt

_addClass :: Text -> Map Text Text -> Map Text Text
_addClass c = Map.alter add "class"
  where
    add Nothing = Just "fragment"
    add (Just cls) = Just $ Text.unwords $ ("fragment" :) $ Text.words cls

instance ToText StaticString where
  toText s = getText s

instance ToText ChoiceString where
  toText (Static s) = toText s
  toText (String s) = toText s
  toText (Text.Blaze.Internal.Text t) = t
  toText (ByteString s) = decodeUtf8 s
  toText (PreEscaped c) = toText c
  toText (External c) = toText c
  toText (AppendChoiceString c1 c2) = toText c1 <> toText c2
  toText EmptyChoiceString = ""

-- just for debugging
instance Prelude.Show StaticString where
  show s = getString s ""

-- just for debugging
instance Prelude.Show ChoiceString where
  show (Static s) = show s
  show (String s) = show s
  show (Text.Blaze.Internal.Text t) = show t
  show (ByteString s) = show s
  show (PreEscaped c) = show c
  show (External c) = show c
  show (AppendChoiceString c1 c2) = show c1 <> show c2
  show EmptyChoiceString = ""

-- just for debugging
instance Prelude.Show (MarkupM a) where
  show (Parent tag open end html) = "Parent" <> show tag <> show open <> show end <> show html
  show (CustomParent string html) = "CustomParent" <> show string <> show html
  show (Leaf tag open end _) = "Leaf" <> show tag <> show open <> show end
  show (CustomLeaf tag what _) = "CustomLeaf" <> show tag <> show what
  show (Content string _) = "Content" <> show string
  show (Comment string _) = "Comment" <> show string
  show (Append html1 html2) = "Append" <> show html1 <> show html2
  show (AddAttribute raw key value html) = "AddAttribute" <> show raw <> show key <> show value <> show html
  show (AddCustomAttribute key value html) = "AddCustomAttribute" <> show key <> show value <> show html
  show (Empty _) = "Empty"

nullA :: Map Text Text
nullA = fromList []

instance ToText (Map Text Text) where
  toText m = Map.foldlWithKey (\s k v -> s <> " " <> k <> "=\"" <> v <> "\"") "" m
