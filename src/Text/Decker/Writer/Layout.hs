{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Writer.Layout (markdownToHtml, writePandocFile) where

import Data.List (lookup)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake
import Relude
import System.FilePath
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (ChoiceString (..), MarkupM (..), StaticString, getString, getText)
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
import qualified Prelude

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

markdownToHtml :: Disposition -> Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtml disp meta getTemplate markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  pandoc@(Pandoc meta _) <- readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerSectionDivs = False,
            writerHighlightStyle = highlightStyle meta,
            writerHTMLMathMethod =
              MathJax (lookupMetaOrElse "" "mathjax-url" meta),
            writerVariables =
              Context $
                fromList
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
  liftIO $ do
    html <-
      runIO (writeHtml4 options {writerTemplate = Nothing} pandoc)
        >>= handleError
    let string = renderHtml $ transformHtml nullA html
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
    runIO (writeHtml5String options (embedMetaMeta (Pandoc meta raw)))
      >>= handleError
      >>= Text.writeFile out

-- | Transforms a HTML structure such that divs with a attribute
-- data-tag=section are transformed into section elements with the data-tag
-- attribute removed. Also, the class "processed" is removed from all elements.
transformHtml :: Map Text Text -> MarkupM a -> MarkupM a
transformHtml attribs m@(Parent tag open end html)
  | getText tag `elem` ["div", "span"] && Map.member "data-tag" attribs =
    let name = toString $ attribs Map.! "data-tag"
     in Parent
          (fromString name)
          (fromString $ "<" <> name)
          (fromString $ "</" <> name <> ">")
          (transformHtml nullA html)
-- dissard the collected attributes and recurse
transformHtml attribs m@(Parent tag open end html) =
  Parent tag open end (transformHtml nullA html)
-- dissard the collected attributes and recurse
transformHtml attribs m@(CustomParent string html) =
  CustomParent string (transformHtml nullA html)
-- just pass through
-- dissard the collected attributes and recurse twice
transformHtml attribs m@(Append html1 html2) =
  Append (transformHtml nullA html1) (transformHtml nullA html2)
-- drop 'processed' from class attribute
transformHtml attribs m@(AddAttribute raw key value html)
  | toText raw == "class" =
    let cls = Text.unlines $ filter (/= "processed") $ Text.lines $ toText value
     in if Text.null cls
          then transformHtml attribs html
          else AddAttribute raw key value (transformHtml (Map.insert (toText raw) cls attribs) html)
-- add the attribute to the map for later retrieval
transformHtml attribs m@(AddAttribute raw key value html) =
  AddAttribute raw key value (transformHtml (Map.insert (toText raw) (toText value) attribs) html)
-- drop the custom data-tag attribute after adding it to the map
transformHtml attribs m@(AddCustomAttribute key value html)
  | toText key == "data-tag" =
    transformHtml (Map.insert (toText key) (toText value) attribs) html
-- drop 'processed' from class attribute
transformHtml attribs m@(AddCustomAttribute key value html)
  | toText key == "class" =
    let cls = Text.unlines $ filter (/= "processed") $ Text.lines $ toText value
     in if Text.null cls
          then transformHtml attribs html
          else AddCustomAttribute key value (transformHtml (Map.insert (toText key) cls attribs) html)
-- add the custom attribute to the map for later retrieval
transformHtml attribs m@(AddCustomAttribute key value html) =
  AddCustomAttribute key value (transformHtml (Map.insert (toText key) (toText value) attribs) html)
transformHtml attribs m = m

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
