{-# LANGUAGE OverloadedStrings #-}

module ShortLinkTests ( shortLinkTests ) where

import Data.ByteString.UTF8 ( fromString )
import Data.Either
import Data.Yaml as Yaml ( Value(..), decodeEither' )

import Test.Hspec

import Text.Decker.Filter.ShortLink
import Text.Decker.Internal.Meta

meta =
    toPandocMeta $
    fromRight Yaml.Null $
    decodeEither' $
    fromString $
    unlines
        [ "short-links:"
        , "  bind:"
        , "    notebook: local"
        , "  notebook:"
        , "    binder: 'https://mybinder.org/v2/gh/monofon/plc-java/master?filepath=@@@'"
        , "    local: 'http://localhost:8192/@@?token=plc'"
        , "  note: 'http://localhost/@@'"
        ]

shortLinkTests =
    do describe "fillTemplate" $
           do it "replaces @@ in template with value" $
                  fillTemplate "ha@@o" "ll" `shouldBe` Just "hallo"
              it "replaces @@ in template with value" $
                  fillTemplate "ha@@o@@o" "ll" `shouldBe`
                  Just "hallollo"
              it "replaces @@ in template with value" $
                  fillTemplate "ha@@" "ll" `shouldBe` Just "hall"
              it "replaces @@ in template with value" $
                  fillTemplate "hallo" "ll" `shouldBe` Just "hallo"
       describe "evalUrl" $
           do it "evaluates the short links" $
                  evalUrl meta "note:hallo" `shouldBe`
                  "http://localhost/hallo"
              it "evaluates the short links" $
                  evalUrl meta "notebook.local:hallo" `shouldBe`
                  "http://localhost:8192/hallo?token=plc"
              it "evaluates the short links with bound templates" $
                  evalUrl meta "notebook:hallo" `shouldBe`
                  "http://localhost:8192/hallo?token=plc"
