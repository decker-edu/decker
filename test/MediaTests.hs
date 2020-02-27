{-# LANGUAGE NoImplicitPrelude #-}

module MediaTests
  ( mediaTests
  ) where

import Text.Decker.Filter.Decker

import Data.Maybe
import Relude
import Test.Hspec

-- import Text.Blaze.Html
-- import Text.Blaze.Html.Renderer.Text
-- import Text.Blaze.Html5 as H
-- import Text.Blaze.Html5.Attributes as A
import Text.Pandoc

-- import qualified Text.URI as URI
-- | Constructs a filter runner with default parameters
testFilter = runFilter' def nullMeta

doFilter :: RawHtml a => (Filter (Maybe a)) -> IO (Maybe a)
doFilter action = fst <$> runStateT action (FilterState def nullMeta)

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

styledCaption = [Str "A", Space, Strong [Str "logo."]]

plainImage =
  (Image
     ( "logo"
     , ["myclass"]
     , [("width", "30%"), ("css:border", "1px"), ("myattribute", "1")])
     []
     ("logo.jpg", ""))

plainImageCaptionedHtml =
  Just $
  RawInline
    (Format "html5")
    "<figure id=\"logo\" class=\"myclass\" data-myattribute=\"1\" style=\"border:1px;width:30%;\"><img data-src=\"logo.jpg\"><figcaption>A <strong>logo.</strong></figcaption></figure>"

plainImageHtml =
  Just $
  RawInline
    (Format "html5")
    "<img id=\"logo\" class=\"myclass\" data-src=\"logo.jpg\" data-myattribute=\"1\" style=\"border:1px;width:30%;\">"

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
