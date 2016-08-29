{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Filter

main :: IO ()
main = toJSONFilter expandMacros

