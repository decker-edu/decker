{-# LANGUAGE NoImplicitPrelude #-}

module MediaTests
  ( mediaTests
  ) where

import Text.Decker.Filter.Decker
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta

import Data.Maybe
import qualified Data.Text.IO as Text
import NeatInterpolation
import Relude
import Test.Hspec as Hspec
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.Walk
import System.Directory

filterMeta = do
  cwd <- toText <$> getCurrentDirectory
  return $
    setTextMetaValue "decker.top-base-dir" cwd $
    setTextMetaValue "decker.base-dir" cwd $
    setTextMetaValue "decker.project-dir" cwd $
    setTextMetaValue "decker.public-dir" cwd $ nullMeta

-- import qualified Text.URI as URI
-- | Constructs a filter runner with default parameters
testFilter b f = do
  meta <- filterMeta 
  runFilter' def meta b f

doFilter :: Filter Inline -> IO Inline
doFilter action = do
  meta <- filterMeta 
  fst <$> runStateT (action) (FilterState def meta)

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
  describe "toHtml" $
    it "transforms Pandoc Blocks and Inlines to Blaze Html" $ do
      renderHtml (toHtml (Str "Hallo")) `shouldBe` "Hallo"
      renderHtml (toHtml (Para [Str "Hallo"])) `shouldBe` "<p>Hallo</p>"
      renderHtml (toHtml [(Str "Hallo"), (Str "Hallo")]) `shouldBe` "HalloHallo"
      renderHtml (toHtml [(Para [Str "Hallo"]), (Para [Str "Hallo"])]) `shouldBe`
        "<p>Hallo</p>\n<p>Hallo</p>"
      renderHtml (toHtml (RawInline (Format "html") "Hallo")) `shouldBe` "Hallo"
  Hspec.runIO $
    writeSnippetReport "doc/media-filter-report-page.md" testSnippets

styledCaption = [Str "A", Space, Strong [Str "logo."]]

plainImage =
  (Image
     ( "logo"
     , ["myclass"]
     , [("width", "30%"), ("css:border", "1px"), ("myattribute", "1")])
     []
     ("/test/decks/include/06-metal.png", ""))

plainImageCaptionedHtml =
  RawInline
    (Format "html")
    "<figure id=\"logo\" class=\"decker myclass\" data-myattribute=\"1\" style=\"width:30%;border:1px;\"><img class=\"decker\" data-src=\"test/decks/include/06-metal.png\"><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

plainImageHtml =
  RawInline
    (Format "html")
    "<img id=\"logo\" class=\"decker myclass\" data-src=\"test/decks/include/06-metal.png\" data-myattribute=\"1\" style=\"width:30%;border:1px;\">"

plainVideo =
  Image
    ( "video"
    , ["myclass", "autoplay", "loop"]
    , [ ("width", "30%")
      , ("css:border", "1px")
      , ("annoying", "100")
      , ("poster", "/test/decks/include/06-metal.png")
      , ("preload", "none")
      , ("start", "23")
      , ("stop", "42")
      ])
    []
    ("/test/decks/pacman-perfect-game.mp4", "")

plainVideoHtml =
  RawInline
    (Format "html")
    "<video id=\"video\" class=\"decker myclass\" data-src=\"test/decks/pacman-perfect-game.mp4#t=23,42\" data-annoying=\"100\" style=\"width:30%;border:1px;\" poster=\"/test/decks/include/06-metal.png\" preload=\"none\" loop=\"1\" data-autoplay=\"1\"></video>"

plainVideoCaptionedHtml =
  RawInline
    (Format "html")
    "<figure id=\"video\" class=\"decker myclass\" data-annoying=\"100\" style=\"width:30%;border:1px;\"><video class=\"decker\" data-src=\"test/decks/pacman-perfect-game.mp4#t=23,42\" poster=\"/test/decks/include/06-metal.png\" preload=\"none\" loop=\"1\" data-autoplay=\"1\"></video><figcaption class=\"decker\">A <strong>logo.</strong></figcaption></figure>"

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
  fMeta <- filterMeta
  pandoc@(Pandoc meta blocks) <-
    handleError (runPure (readMarkdown readerOptions markdown))
  filtered@(Pandoc fmeta _) <-
    mediaFilter
      def
      (Pandoc (setBoolMetaValue "decker.filter.pretty" True fMeta) blocks)
  handleError $
    runPure $ writeHtml5String writerOptions $ walk dropPara filtered

dropPara (Para inlines) = Plain inlines
dropPara block = block

testSnippets :: [(Text, Text, Text)]
testSnippets =
  [ ( "Plain image"
    , "An image that is used inline in a paragraph of text."
    , "![$e=mc^2$](/test/decks/include/06-metal.png)")
  , ( "SVG image"
    , "An SVG image that is embedded into the HTML document."
    , "![](/test/decks/empty.svg){.embed css:background-color=\"magenta\"}")
  , ( "Embedded PDF"
    , "A PDF document that is embedded through an object tag."
    , "![](https://adobe.com/some.pdf)")
  , ( "Plain image with caption"
    , "An image with a caption. The image is surrounded by a figure element."
    , [text|
        ![Caption.](/test/decks/include/06-metal.png)

        This is not a caption, but the next paragraph.
      |])
  , ( "Plain image with caption"
    , "An image with a caption. The image is surrounded by a figure element."
    , [text|
        ![](/test/decks/include/06-metal.png)

        Caption: Caption.
      |])
  , ( "Plain image with URL query"
    , "Query string and fragment identifier in URLs are preserved."
    , "![Caption.](https://some.where/image.png&key=value)")
  , ( "Plain image with size attributes."
    , " Percentage values for `width` and `height` are transfered to the figure element, other values go to the image element."
    , "![Caption.](/test/decks/include/06-metal.png){width=\"40%\"}")
  , ( "Plain image with size attributes."
    , " Percentage values for `width` and `height` are transfered to the figure element, other values go to the image element."
    , "![Caption.](/test/decks/include/06-metal.png){height=\"200px\"}")
  , ( "Plain image with size attributes."
    , " Percentage values for `width` and `height` are transfered to the figure element, other values go to the image element."
    , "![Caption.](/test/decks/include/06-metal.png){height=\"200px\" width=\"40%\"}")
  , ( "Plain image with custom attributes."
    , "Image attributes are handled in complex ways."
    , "![Caption.](/test/decks/include/06-metal.png){#myid .myclass width=\"40%\" css:border=\"1px\" css:background-color=\"magenta\" myattribute=\"value\"}")
  , ( "Plain audio"
    , "Images that are audio clips are converted to an audio tag."
    , "![Caption.](test/decks/audio.mp3){.autoplay .controls}")
  , ( "Plain video"
    , "Images that are videos are converted to a video tag."
    , "![Caption.](test/decks/pacman-perfect-game.mp4){width=\"42%\"}")
  , ( "Plain video with Media Fragments URI"
    , "A local video with start time."
    , "![Caption.](test/decks/pacman-perfect-game.mp4){start=\"5\" stop=\"30\" preload=\"none\"}")
  , ( "Plain video with specific attributes"
    , "Video tag specific classes are translated to specific attributes."
    , "![Caption.](test/decks/pacman-perfect-game.mp4){.controls .autoplay start=\"5\" stop=\"30\" poster=\"/test/decks/include/06-metal.png\" preload=\"none\"}")
  , ( "Three images in a row"
    , "Line blocks filled with only image tags are translated to a row of images. Supposed to be used with a flexbox masonry CSS layout."
    , [text|
        | ![](/test/decks/include/06-metal.png)
        | ![Caption.](test/decks/pacman-perfect-game.mp4){.autoplay}
        | ![](/test/decks/include/06-metal.png){css:border="1px solid black"}

      |])
  , ( "Four images in a row with caption"
    , "Line blocks filled with only image tags are translated to a row of images. Supposed to be used with a flexbox masonry CSS layout."
    , [text|
        | ![](/test/decks/include/06-metal.png)
        | ![](test/decks/pacman-perfect-game.mp4){.autoplay}
        | ![](/test/decks/include/06-metal.png){css:border="1px solid black"}
        | ![](/test/decks/include/06-metal.png)

        Caption: Caption
      |])
  , ( "Iframe with caption"
    , "A simple iframe with a caption. The URL can be a top level domain because the `iframe` class is specified."
    , "![Caption.](https://www.heise.de/){.iframe}")
  , ( "Iframe with custom attributes and query string"
    , "A simple iframe with custom attributes and a query string that are both transfered correctly."
    , "![Caption.](https://www.heise.de/index.html#some-frag?token=83fd3d4){height=\"400px\" model=\"some-stupid-ass-model.off\" lasersword=\"off\"}")
  , ( "Mario's model viewer"
    , "A simple iframe with a special url."
    , "![Caption.](http://3d.de/model.off){.mario height=\"400px\" phasers=\"stun\"}")
  , ( "Youtube video stream"
    , "An image with source URL scheme `youtube:` results in an embedded video player."
    , "![](youtube:1234567890){#video1 .autoplay .controls width=\"75%\"}")
  , ( "Vimeo it baby"
    , "An image with source URL scheme `vimeo:` results in an embedded video player."
    , "![Caption.](vimeo://1234567890){#video2 .some-class autoplay=\"1\" aspect=\"4:3\" width=\"75%\" some-attribute=\"yeah\"}")
  , ( "Twitch it baby"
    , "An image with source URL scheme `twitch:` results in an embedded video player."
    , "![Caption.](twitch:1234567890){.autoplay .controls aspect=\"5:3\" width=\"75%\"}")
  , ( "Background image"
    , "The last image in a level 1 header is promoted to the slide background."
    , "# Background Image ![](/test/decks/include/06-metal.png){size=\"cover\"}")
  , ( "Background video"
    , "The last image in a level 1 header is promoted to the slide background."
    , "# Background Image ![](test/decks/pacman-perfect-game.mp4){.loop .muted color=\"black\"}")
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
