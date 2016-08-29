#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Default           ()
import           Data.List.Split
import           Text.Pandoc.Definition ()
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import           Text.Printf
import Filter

main :: IO ()
main = toJSONFilter filterNotes

