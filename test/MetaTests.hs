{-# LANGUAGE OverloadedStrings #-}

module MetaTests
  ( metaTests
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as Text

import Test.Hspec

import Text.Decker.Internal.Meta
import Text.Pandoc hiding (lookupMeta)

m1 =
  Meta
    (M.fromList
       [ ("bool", MetaBool True)
       , ( "write-back"
         , MetaMap
             (M.fromList
                [ ("line-columns", MetaString "80")
                , ("line-wrap", MetaString "none")
                ]))
       , ("top", MetaMap (M.fromList [("bool", MetaBool True)]))
       , ( "list"
         , MetaList
             [ MetaMap (M.fromList [("bool0", MetaBool True)])
             , MetaMap (M.fromList [("bool1", MetaBool True)])
             , MetaMap (M.fromList [("bool2", MetaBool True)])
             ])
       ])

m2 =
  Meta (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "0")]))])

m2' =
  Meta (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "1")]))])

m3 = Meta (M.fromList [("list", MetaList [MetaString "some/where/img.png"])])

m3' =
  Meta
    (M.fromList
       [ ( "list"
         , MetaList [MetaString "img.png", MetaString "some/where/img.png"])
       ])

m3'' =
  Meta
    (M.fromList
       [ ( "list"
         , MetaList [MetaString "img.jpg", MetaString "some/where/img.png"])
       ])

m4 =
  Meta
    (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "brot")]))])

m4' =
  Meta
    (M.fromList [("level1", MetaMap (M.fromList [("one", MetaString "BROT")]))])

m4'' = Meta (M.fromList [("level1", MetaString "Gone.")])

m5 =
  Meta
    (M.fromList
       [ ( "level1"
         , MetaMap
             (M.fromList
                [("one", MetaList [MetaString "brot", MetaString "butter"])]))
       ])

m5' =
  Meta
    (M.fromList
       [ ( "level1"
         , MetaMap
             (M.fromList
                [("one", MetaList [MetaString "BROT", MetaString "BUTTER"])]))
       ])

m6 =
  Meta
    (M.fromList
       [ ( "level1"
         , MetaMap
             (M.fromList
                [ ("one", MetaList [MetaString "brot", MetaString "butter"])
                , ("two", MetaString "salz")
                ]))
       ])

m6' =
  Meta
    (M.fromList
       [ ( "level1"
         , MetaMap
             (M.fromList
                [ ("one", MetaList [MetaString "BROT", MetaString "BUTTER"])
                , ("two", MetaString "SALZ")
                ]))
       ])

m7 = Meta (M.fromList [])

m7' = Meta (M.fromList [("toast", MetaString "TOAST")])

metaTests = do
  describe "getMetaBool" $ do
    it "looks up a top-level boolean meta value" $
      lookupMeta "bool" m1 `shouldBe` Just True
    it "looks up a top-level boolean meta value" $
      lookupMeta "none" m1 `shouldBe` (Nothing :: Maybe Bool)
    it "looks up a boolean meta value" $
      lookupMeta "top.bool" m1 `shouldBe` Just True
    it "looks up a boolean meta value" $
      lookupMeta "top.none" m1 `shouldBe` (Nothing :: Maybe Bool)
    it "looks up a boolean meta value in list" $
      lookupMeta "list[2].bool2" m1 `shouldBe` Just True
  describe "getMetaInt" $
    it "looks up a top-level int meta value" $
    lookupMeta "write-back.line-columns" m1 `shouldBe` Just (80 :: Int)
  describe "getMetaString" $
    it "looks up a top-level int meta value" $
    lookupMeta "write-back.line-wrap" m1 `shouldBe` Just ("none" :: String)
  describe "setMetaValue" $ do
    it "sets the value in a nested map" $
      setMetaValue "level1.one" (MetaString "1") m2 `shouldBe` m2'
    -- it "sets the value in a nested map with arrays" $
    --   setMetaValue "list[0]" (MetaString "img.jpg") m3' `shouldBe` m3''
    it "sets the value in a nested map" $
      addMetaValue "list" (MetaString "img.png") m3 `shouldBe` m3'
  describe "adjustMetaValue" $ do
    it "adjusts a value in a nested map" $
      adjustMetaValue upCase "level1.one" m4 `shouldBe` m4'
    it "does not touch anything that is not there" $
      adjustMetaValue upCase "level1.two" m4 `shouldBe` m4
    it "does work on the top level" $
      adjustMetaValue replace "level1" m4 `shouldBe` m4''
    it "does work on list values" $
      adjustMetaValue upCase "level1.one" m5 `shouldBe` m5'
  describe "adjustMetaValueM" $ do
    it "does adjust a value in a nested map" $
      adjustMetaValueM upCaseM "level1.one" m4 `shouldReturn` m4'
    it "does not touch anything that is not there" $
      adjustMetaValueM upCaseM "level1.two" m4 `shouldReturn` m4
    it "does work on the top level" $
      adjustMetaValueM replaceM "level1" m4 `shouldReturn` m4''
    it "does work on list values" $
      adjustMetaValueM upCaseM "level1.one" m5 `shouldReturn` m5'
  describe "adjustMetaStringsBelowM" $ do
    it "does adjust all string-ish values below the key" $
      adjustMetaStringsBelowM upCaseTextM "level1" m6 `shouldReturn` m6'
  describe "addMetaKeyValue" $ do
    it "adds a key value pair to the named dictionary" $
      addMetaKeyValue "toast" (MetaString "TOAST") m7 `shouldBe` m7'

replace :: MetaValue -> MetaValue
replace _ = (MetaString "Gone.")

replaceM :: MetaValue -> IO MetaValue
replaceM _ = return (MetaString "Gone.")

upCase :: MetaValue -> MetaValue
upCase (MetaString text) = MetaString $ Text.toUpper text
upCase (MetaList list) = MetaList $ map upCase list
upCase value = value

upCaseM :: MetaValue -> IO MetaValue
upCaseM (MetaString text) = return $ MetaString $ Text.toUpper text
upCaseM (MetaList list) = MetaList <$> mapM upCaseM list
upCaseM value = return value

upCaseTextM = return . Text.toUpper
