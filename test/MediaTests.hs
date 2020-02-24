module MediaTests
  ( mediaTests
  ) where

import Text.Decker.Filter.Decker

import Test.Hspec
import Text.Pandoc

-- | Constructs a filter runner with default parameters
testFilter = runFilter' def nullMeta

mediaTests =
  describe "pairwise" $
  it "pairwise matches a list of walkables" $ do
    testFilter filter0 blockAin `shouldReturn` blockAin
    testFilter filter1 blockAin `shouldReturn` blockAin
    testFilter filter2 blockAin `shouldReturn` blockAout
    testFilter filter2 blockBin `shouldReturn` blockBout

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
