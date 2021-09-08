{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Media where

compileImageToInline :: Inline -> Filter Inline

compileImageToBlock :: Inline -> Filter Block

compileImageToBlock :: [Inline] -> Filter Block

compileCodeToBlock :: Inline -> Filter Block
