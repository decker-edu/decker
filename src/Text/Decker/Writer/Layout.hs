{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Writer.Layout (markdownToHtmlLayoutDeck) where

import Control.Exception
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Debug.Pretty.Simple
import Development.Shake
import Relude
import Relude.String.Conversion (ConvertUtf8 (decodeUtf8Strict))
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal (ChoiceString (..), Markup, MarkupM (..), StaticString, getString, getText)
import Text.Decker.Filter.Filter
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
import Text.Pretty.Simple
import qualified Prelude

markdownToHtmlLayoutDeck :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlLayoutDeck meta getTemplate markdownFile out = do
  putNormal "EXPERIMENTAL LAYOUT WRITER"
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let disp = Disposition Deck Html
  pandoc@(Pandoc meta _) <- readAndFilterMarkdownFile disp meta markdownFile
  let highlightStyle =
        case lookupMeta "highlightjs" meta of
          Nothing -> Just pygments
          Just (_ :: Text) -> Nothing
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1,
            writerSectionDivs = False,
            writerTemplate = Just template,
            writerHighlightStyle = highlightStyle,
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
  writePandocFile "revealjs" options out pandoc

writePandocFile :: Text -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile format options out pandoc@(Pandoc meta blocks) =
  liftIO $ do
    html <-
      runIO (writeHtml5 def pandoc)
        >>= handleError
    let raw = RawBlock "html" $ fromLazy $ renderHtml $ transformHtml nullA html
    runIO (writeRevealJs options (embedMetaMeta (Pandoc meta [raw])))
      >>= handleError
      >>= Text.writeFile out

transformHtml :: Map Text Text -> MarkupM a -> MarkupM a
transformHtml attribs m@(Parent tag open end html)
  | getText tag == "div"
      && Map.lookup "data-tag" attribs == Just "section" =
    Parent
      "section"
      "<section"
      "</section>"
      (transformHtml nullA html)
transformHtml attribs m@(Parent tag open end html) =
  Parent tag open end (transformHtml nullA html)
transformHtml attribs m@(CustomParent string html) =
  CustomParent string (transformHtml nullA html)
transformHtml attribs m@(Leaf tag open end value) =
  Leaf tag open end value
transformHtml attribs m@(CustomLeaf tag what value) =
  CustomLeaf tag what value
transformHtml attribs m@(Content string value) =
  Content string value
transformHtml attribs m@(Comment string value) =
  Comment string value
transformHtml attribs m@(Append html1 html2) =
  Append (transformHtml attribs html1) (transformHtml attribs html2)
transformHtml attribs m@(AddAttribute raw key value html) =
  AddAttribute raw key value (transformHtml (Map.insert (toText raw) (toText value) attribs) html)
transformHtml attribs m@(AddCustomAttribute key value html) =
  AddCustomAttribute key value (transformHtml attribs html)
transformHtml attribs m@(Empty html) =
  Empty html

instance Prelude.Show StaticString where
  show s = getString s ""

instance Prelude.Show ChoiceString where
  show (Static s) = show s
  show (String s) = show s
  show (Text.Blaze.Internal.Text t) = show t
  show (ByteString s) = show s
  show (PreEscaped c) = show c
  show (External c) = show c
  show (AppendChoiceString c1 c2) = show c1 <> show c2
  show EmptyChoiceString = ""

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

data Node
  = TagNode
      { tag :: Text,
        closed :: Bool,
        attribs :: Map Text Text,
        children :: [Node]
      }
  | TextNode
      { attribs :: Map Text Text,
        text :: Text
      }
  deriving (Show)

toDOM :: forall a. MarkupM a -> Node
toDOM = collapseTextNodes . TagNode "div" True nullA . toDOM' (fromList [])

nullA :: Map Text Text
nullA = fromList []

toDOM' :: Map Text Text -> MarkupM a -> [Node]
toDOM' as (Parent tag open end html) =
  [TagNode (toText tag) (toText tag `notElem` singletons) as (toDOM' nullA html)]
toDOM' as (CustomParent string html) =
  throw $ AssertionFailed $ toString $ "CustomParent" <> toText string
toDOM' as (Leaf tag open end _) =
  [TagNode (toText tag) (toText tag `notElem` singletons) as []]
toDOM' as (CustomLeaf tag what _) =
  [TagNode (toText tag) (toText tag `notElem` singletons) as []]
toDOM' as (Content string _) =
  [TextNode as (toText string)]
toDOM' as (Comment string _) =
  [TextNode as (toText string)]
toDOM' as (Append (Content a _) (Content b _)) =
  [TextNode nullA (toText a <> toText b)]
toDOM' as (Append html1 html2) =
  toDOM' nullA html1 <> toDOM' nullA html2
toDOM' as (AddAttribute raw key value html) =
  toDOM' (Map.insert (toText raw) (toText value) as) html
toDOM' as (AddCustomAttribute key value html) =
  toDOM' (Map.insert (toText key) (toText value) as) html
toDOM' as (Empty _) = []

collapseTextNodes :: Node -> Node
collapseTextNodes tn@TagNode {} =
  let collapsed = map collapseTextNodes $ collapse (children tn)
   in tn {children = collapsed}
collapseTextNodes n = n

collapse :: [Node] -> [Node]
collapse ns = foldl' f [] ns
  where
    f [] n = [n]
    f ((TextNode a1 t1) : r) (TextNode a2 t2) | a1 == a2 = TextNode a1 (t1 <> t2) : r
    f ns n = n : ns

singletons =
  [ "area",
    "base",
    "br",
    "col",
    "command",
    "embed",
    "hr",
    "img",
    "input",
    "keygen",
    "link",
    "menuitem",
    "meta",
    "param",
    "source",
    "track",
    "wbr"
  ]

instance ToText (Map Text Text) where
  toText m = Map.foldlWithKey (\s k v -> s <> " " <> k <> "=\"" <> v <> "\"") "" m

instance ToText Node where
  toText (TagNode t True a cs) = Text.concat ["<", t, toText a, ">", Text.concat $ map toText cs, "</", t, ">"]
  toText (TagNode t False a _) = Text.concat ["<", t, toText a, ">"]
  toText (TextNode a t) = t