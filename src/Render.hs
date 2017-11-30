{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderCodeBlocks
  , renderedCodeExtensions
  ) where

import CRC32
import Context
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake
import Extra
import Project
import System.Directory
       (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Printf

-- | Evaluate code blocks
renderCodeBlocks :: Pandoc -> Action Pandoc
renderCodeBlocks pandoc =
  walk maybeRenderImage <$> walkM maybeRenderCodeBlock pandoc

data Processor = Processor
  { extensions :: [String]
  } deriving (Show)

renderClass :: String
renderClass = "render"

dotPrelude :: String -> String
dotPrelude _ = ""

gnuplotPrelude :: String -> String
gnuplotPrelude _ = "set terminal svg;"

processors :: Map.Map String Processor
processors =
  Map.fromList
    [ ("dot", Processor [".dot"])
    , ("gnuplot", Processor [".gnuplot", ".gpi", ".plt", "gp"])
    , ("tikz", Processor [".tex", ".latex"])
    ]

renderedCodeExtensions :: [String]
renderedCodeExtensions = Map.foldr (\p es -> (extensions p) ++ es) [] processors

findProcessor :: [String] -> Maybe Processor
findProcessor classes =
  if renderClass `elem` classes
    then listToMaybe $ Map.elems matching
    else Nothing
  where
    matching = Map.restrictKeys processors (Set.fromList classes)

maybeRenderImage :: Inline -> Inline
maybeRenderImage image@(Image (id, classes, namevals) inlines (url, title)) =
  if takeExtension url `elem` [".dot", ".gnuplot", ".tex"]
    then let svgFile = url <.> "svg"
         in Image (id, classes, namevals) inlines (svgFile, title)
    else image
maybeRenderImage inline = inline

maybeRenderCodeBlock :: Block -> Action Block
maybeRenderCodeBlock code@(CodeBlock (id, classes, namevals) contents) =
  case findProcessor classes of
    Just processor -> do
      svgFile <-
        extractCodeIfChanged "" ((head . extensions) processor) contents
      return $
        Para
          [ Image
              (id, classes, namevals)
              []
              (svgFile, "Generated from embedded code block")
          ]
    Nothing -> return code
maybeRenderCodeBlock block = return block

-- | Encode a svg snippet into a data url for an image element
svgDataUrl :: String -> String
svgDataUrl svg =
  "data:image/svg+xml;base64," ++ (B.unpack (B64.encode (B.pack svg)))

extractCodeIfChanged :: String -> FilePath -> FilePath -> Action FilePath
extractCodeIfChanged basename extension code = do
  projectDir <- project <$> getProjectDirs
  let crc = printf "%08x" (calc_crc32 code)
  let basepath =
        "generated" </>
        (concat $ intersperse "-" [defaultString "code" basename, crc])
  let sourceFile = projectDir </> basepath <.> extension
  let svgFile = projectDir </> basepath <.> extension <.> ".svg"
  publicResource <- getPublicResource
  withResource publicResource 1 $
    liftIO $
    unlessM (System.Directory.doesFileExist svgFile) $ do
      createDirectoryIfMissing True (takeDirectory sourceFile)
      writeFile sourceFile code
  need [svgFile]
  return svgFile

defaultString :: String -> String -> String
defaultString d str | null str = d
defaultString _ str = str
