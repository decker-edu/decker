{-# LANGUAGE NoImplicitPrelude #-}

module MediaTests
  ( mediaTests
  ) where

import Text.Decker.Filter.Decker
import Text.Decker.Internal.Meta

import Text.Blaze.Html hiding (text)

import Data.Maybe
import qualified Data.Text.IO as Text
import NeatInterpolation
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

doFilter :: Filter Html -> IO Inline
doFilter action =
  fst <$> runStateT (action >>= renderHtml) (FilterState def filterMeta)

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
    "<figure id=\"logo\" class=\"decker myclass\" data-myattribute=\"1\" style=\"width:30%;border:1px;\"><img class=\"decker\" data-src=\"logo.jpg\" style=\"width:100%;\"><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

plainImageHtml =
  RawInline
    (Format "html5")
    "<img id=\"logo\" class=\"decker myclass\" data-src=\"logo.jpg\" data-myattribute=\"1\" style=\"width:30%;border:1px;\">"

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
    "<video id=\"video\" class=\"decker myclass\" data-src=\"cat.mp4#t=23,42\" data-annoying=\"100\" style=\"width:30%;border:1px;\" poster=\"some/where/image.png\" preload=\"none\" autoplay=\"1\" loop=\"1\"></video>"

plainVideoCaptionedHtml =
  RawInline
    (Format "html5")
    "<figure id=\"video\" class=\"decker myclass\" data-annoying=\"100\" style=\"width:30%;border:1px;\"><video class=\"decker\" data-src=\"cat.mp4#t=23,42\" style=\"width:100%;\" poster=\"some/where/image.png\" preload=\"none\" autoplay=\"1\" loop=\"1\"></video><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

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

writerOptions = def {writerExtensions = pandocExtensions}

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
  handleError (runPure (writeHtml5String writerOptions filtered))

testSnippets :: [(Text, Text, Text)]
testSnippets =
  [ ( "Plain image"
    , "An image that is used inline in paragraph of text."
    , "Inline ![](/some/path/image.png)")
  , ( "Plain image with caption"
    , "An image with a caption. The image is surrounded by a figure element."
    , "Inline ![This is a plain image.](path/image.png)")
  , ( "Plain image with URL query"
    , "Query string and fragment identifier in URLs are preserved."
    , "Inline ![Image URI with query string.](https://some.where/image.png&key=value)")
  , ( "Plain image with custom attributes."
    , "Image attributes are handled in complex ways."
    , "Inline ![Image with attributes](/some/path/image.png){#myid .myclass width=\"40%\" css:border=\"1px\" myattribute=\"value\"}")
  , ( "Plain video"
    , "Images that are videos are converted to a video tag."
    , "Inline ![A local video.](/some/path/video.mp4){width=\"42%\"}")
  , ( "Plain video with Media Fragments URI"
    , "Description"
    , "Inline ![A local video with start time.](/some/path/video.mp4){start=\"5\" stop=\"30\" preload=\"none\"}")
  , ( "Plain video with specific attributes"
    , "Video tag specific classes are translated to specific attributes."
    , "Inline ![A local video with all features on.](/some/path/video.mp4){.controls .autoplay start=\"5\" stop=\"30\" poster=\"somewhere/image.png\" preload=\"none\"}")
  , ( "Three images in a row"
    , "Line blocks filled with only image tags are translated to a row of images. Supposed to be used with a flexbox masonry CSS layout."
    , [text|
        | ![](image.png)
        | ![Not an image](movie.mp4){.autoplay}
        | ![](image.png){css:border=\"1px black\"}

      |])
  , ( "Iframe with caption"
    , "A simple iframe with a caption. The URL can be a top level domain because the `iframe` class is specified."
    , "![Caption](https://www.heise.de/){.iframe}")
  , ( "Iframe with custom attributes and query string"
    , "A simple iframe with custom attributes and a query string that are both transfered correctly."
    , "![Caption](https://www.heise.de/index.html#some-frag?token=83fd3d4){height=\"400px\" model=\"some-stupid-ass-model.off\" lasersword=\"off\"}")
  , ( "Mario's model viewer"
    , "A simple iframe with a special url."
    , "![Caption](http://3d.de/model.off){.mario height=\"400px\" phasers=\"stun\"}")
  ]

runSnippets :: [(Text, Text, Text)] -> IO [(Text, Text, Text, Text)]
runSnippets snippets =
  mapM (\(t, d, s) -> (t, d, s, ) <$> compileSnippet s) snippets

markdownTemplate = "\n$titleblock$\n\n$body$\n"

writeSnippetReport :: FilePath -> [(Text, Text, Text)] -> IO ()
writeSnippetReport file snippets = do
  result <- runSnippets snippets
  template <- either (error . toText) id <$> compileTemplate "" markdownTemplate
  let pandoc = render result
  html <-
    handleError $
    runPure $
    writeMarkdown
      (def
         { writerTemplate = Just template
         , writerExtensions = pandocExtensions
         , writerHighlightStyle = Just pygments
         })
      pandoc
  Text.writeFile file html
  where
    render result =
      Pandoc
        (Meta
           (fromList [("title", MetaString "Decker Media Filter - Test Report")]))
        [ Header 1 nullAttr [Str "Introduction"]
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
        (\(t, d, s, r) ->
           [ HorizontalRule
           , Header 2 nullAttr [Str t]
           , Para [Str d]
           , CodeBlock ("", ["markdown"], []) s
           , Para [Str "translates to"]
           , CodeBlock ("", ["html"], []) r
           ])
        list
