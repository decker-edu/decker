{-# LANGUAGE NoImplicitPrelude #-}

module MediaTests
  ( mediaTests
  ) where

import Text.Decker.Filter.Decker
import Text.Decker.Internal.Meta

-- import Text.Blaze.Html
-- import Text.Blaze.Html.Renderer.Text
-- import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
import Data.Maybe
import qualified Data.Text.IO as Text
import Relude
import Test.Hspec as Hspec
import Text.Pandoc
import Text.Pandoc.Highlighting

filterMeta =
  setTextMetaValue "decker.base-dir" "." $
  setTextMetaValue "decker.project-dir" "/tmp/decker" $
  setTextMetaValue "decker.public-dir" "/tmp/decker/public" $ nullMeta

-- import qualified Text.URI as URI
-- | Constructs a filter runner with default parameters
testFilter = runFilter' def filterMeta

doFilter :: RawHtml a => Filter a -> IO a
doFilter action = fst <$> runStateT action (FilterState def filterMeta)

mediaTests = do
  describe "pairwise" $
    it "pairwise matches a list of walkables" $ do
      testFilter filter0 blockAin `shouldReturn` blockAin
      testFilter filter1 blockAin `shouldReturn` blockAin
      testFilter filter2 blockAin `shouldReturn` blockAout
      testFilter filter2 blockBin `shouldReturn` blockBout
  describe "transformImage" $
    it "plain Pandoc image -> plain HTML image" $ do
      doFilter (transformImage plainImage []) `shouldReturn` plainImageHtml
      doFilter (transformImage plainImage styledCaption) `shouldReturn`
        plainImageCaptionedHtml
      doFilter (transformImage plainVideo []) `shouldReturn` plainVideoHtml
      doFilter (transformImage plainVideo styledCaption) `shouldReturn`
        plainVideoCaptionedHtml
  Hspec.runIO $
    writeSnippetReport "doc/media-filter-report-page.md" testSnippets

styledCaption = [Str "A", Space, Strong [Str "logo."]]

plainImage =
  (Image
     ( "logo"
     , ["myclass"]
     , [("width", "30%"), ("css:border", "1px"), ("myattribute", "1")])
     []
     ("logo.jpg", ""))

plainImageCaptionedHtml =
  RawInline
    (Format "html5")
    "<figure id=\"logo\" class=\"decker myclass\" data-myattribute=\"1\" style=\"border:1px;width:30%;\"><img class=\"decker\" data-src=\"logo.jpg\"><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

plainImageHtml =
  RawInline
    (Format "html5")
    "<img id=\"logo\" class=\"decker myclass\" data-src=\"logo.jpg\" data-myattribute=\"1\" style=\"border:1px;width:30%;\">"

plainVideo =
  Image
    ( "video"
    , ["myclass", "autoplay", "loop"]
    , [ ("width", "30%")
      , ("css:border", "1px")
      , ("annoying", "100")
      , ("poster", "some/where/image.png")
      , ("preload", "none")
      , ("start", "23")
      , ("stop", "42")
      ])
    []
    ("cat.mp4", "")

plainVideoHtml =
  RawInline
    (Format "html5")
    "<video id=\"video\" class=\"decker myclass\" data-src=\"cat.mp4#t=23,42\" data-annoying=\"100\" style=\"border:1px;width:30%;\" poster=\"some/where/image.png\" preload=\"none\" autoplay=\"1\" loop=\"1\"></video>"

plainVideoCaptionedHtml =
  RawInline
    (Format "html5")
    "<figure id=\"video\" class=\"decker myclass\" data-annoying=\"100\" style=\"border:1px;width:30%;\"><video class=\"decker\" data-src=\"cat.mp4#t=23,42\" poster=\"some/where/image.png\" preload=\"none\" autoplay=\"1\" loop=\"1\"></video><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

blockAin = [Para [], Para [Image nullAttr [] ("", "")], Para []]

blockAout = [Para [], RawBlock (Format "html5") ""]

blockBin =
  [ Para []
  , Para [Image nullAttr [] ("", "")]
  , Para []
  , Div nullAttr [Para [], Para [Image nullAttr [] ("", "")], Para []]
  ]

blockBout =
  [ Para []
  , RawBlock (Format "html5") ""
  , Div nullAttr [Para [], RawBlock (Format "html5") ""]
  ]

filter0 :: [Block] -> Filter [Block]
filter0 = pairwise filter
  where
    filter (x, y) = return Nothing

filter1 :: [Block] -> Filter [Block]
filter1 = pairwise filter
  where
    filter (Para [], Para []) = return $ Just [RawBlock (Format "html5") ""]
    filter (x, y) = return Nothing

filter2 :: [Block] -> Filter [Block]
filter2 = pairwise filter
  where
    filter (Para [Image {}], Para []) =
      return $ Just [RawBlock (Format "html5") ""]
    filter (x, y) = return Nothing

readerOptions =
  def
    {readerExtensions = disableExtension Ext_implicit_figures pandocExtensions}

setPretty (Pandoc meta blocks) =
  Pandoc
    (Meta $
     fromList
       [ ( "decker"
         , MetaMap $
           fromList [("filter", MetaMap $ fromList [("pretty", MetaBool True)])])
       ])
    blocks

compileSnippet :: Text -> IO Text
compileSnippet markdown = do
  pandoc@(Pandoc meta blocks) <-
    handleError (runPure (readMarkdown readerOptions markdown))
  filtered@(Pandoc fmeta _) <-
    mediaFilter
      def
      (Pandoc (setBoolMetaValue "decker.filter.pretty" True filterMeta) blocks)
  print "-----------"
  print $ unMeta fmeta
  handleError (runPure (writeHtml5String def filtered))

testSnippets :: [Text]
testSnippets =
  [ "Inline ![](/some/path/image.png)"
  , "Inline ![This is a **plain** image.](path/image.png)"
  , "Block\n\n![](https://heise.de/logo.png)\n\nImage: This is a **plain** image."
  , "Inline ![Image URI with **query string**.](https:/some.where/image.png&key=value)"
  , "Inline ![Image with **attributes**](/some/path/image.png){#myid .myclass width=\"40%\" css:border=\"1px\" myattribute=\"value\"}"
  , "Inline ![A local video.](/some/path/video.mp4){width=\"42%\"}"
  , "Inline ![A local video with start time.](/some/path/video.mp4){start=\"5\" stop=\"30\" preload=\"none\"}"
  , "Inline ![A local video with all features on.](/some/path/video.mp4){.controls .autoplay start=\"5\" stop=\"30\" poster=\"somewhere/image.png\" preload=\"none\"}"
  ]

runSnippets :: [Text] -> IO [(Text, Text)]
runSnippets snippets = do
  html <- mapM compileSnippet snippets
  return $ zip snippets html

writeSnippetReport :: FilePath -> [Text] -> IO ()
writeSnippetReport file snippets = do
  result <- runSnippets snippets
  let pandoc = render result
  html <-
    handleError $
    runPure $
    writeMarkdown
      (def
         { writerExtensions = pandocExtensions
         , writerHighlightStyle = Just pygments
         })
      pandoc
  Text.writeFile file html
  where
    render result =
      Pandoc
        nullMeta
        [ Header 1 nullAttr [Str "Decker Media Filter - Test Report"]
        , Para
            [ Str "This report is generated during testing and shows "
            , Str "the HTML output for a representative selection of "
            , Str "image tags. It is used for debugging and is the "
            , Str "authoritative reference for CSS authors."
            ]
        , render' result
        ]
    render' list =
      Div nullAttr $
      concatMap
        (\(s, r) ->
           [ HorizontalRule
           , CodeBlock ("", ["markdown"], []) s
           , Para [Str "translates to"]
           , CodeBlock ("", ["html"], []) r
           ])
        list
