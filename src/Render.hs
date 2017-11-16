{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderCodeBlocks
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake
import System.Exit
import System.Process
import Text.Pandoc
import Text.Pandoc.Walk

-- | Evaluate code blocks
renderCodeBlocks :: Pandoc -> Action Pandoc
renderCodeBlocks pandoc = liftIO $ walkM maybeRenderCodeBlock pandoc

data Processor = Processor
  { command :: String
  , args :: [String]
  , prelude :: FilePath -> String
  }

renderClass = "render"

dotPrelude _ = ""

gnuplotPrelude _ =
  "set terminal svg;"

processors =
  Map.fromList
    [ ("dot", Processor "dot" ["-Tsvg"] dotPrelude)
    , ("gnuplot", Processor "gnuplot" ["-d"] gnuplotPrelude)
    ]

findProcessor :: [String] -> t -> Maybe Processor
findProcessor classes namevals =
  if renderClass `elem` classes
    then listToMaybe $ Map.elems matching
    else Nothing
  where
    matching = Map.restrictKeys processors (Set.fromList classes)

maybeRenderCodeBlock :: Block -> IO Block
maybeRenderCodeBlock code@(CodeBlock (id, classes, namevals) contents) =
  case findProcessor classes namevals of
    Just processor -> renderCodeBlock processor code
    Nothing -> return code
maybeRenderCodeBlock block = return block

renderCodeBlock (Processor command args prelude) (CodeBlock (id, classes, namevals) contents) = do
  result <-
    liftIO $ readProcessWithExitCode command args ((prelude "") ++ contents)
  case result of
    (ExitSuccess, svg, _) ->
      return $
      Para
        [ Image
            (id, classes, namevals)
            []
            (svgDataUrl svg, "Generated from code block")
        ]
    (ExitFailure exitCode, _, err) ->
      return $ Para [Str ("Error running " ++ command ++ ": " ++ err)]

-- | Encode a svg snippet into a data url for an image element
svgDataUrl :: String -> String
svgDataUrl svg =
  "data:image/svg+xml;base64," ++ (B.unpack (B64.encode (B.pack svg)))
