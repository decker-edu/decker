#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Filter
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter makeSlides
