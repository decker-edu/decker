{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderCodeBlocks
  , renderedCodeExtensions
  ) where

import CRC32
import Common
import Control.Monad.Trans.Class
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Printf

-- | Evaluate code blocks
renderCodeBlocks :: Pandoc -> Decker Pandoc
renderCodeBlocks pandoc =
  walk maybeRenderImage <$> walkM maybeRenderCodeBlock pandoc

data Processor = Processor
  { extensions :: [String]
  , preamble :: String
  , postamble :: String
  } deriving (Show)

renderClass :: String
renderClass = "render"

dotPrelude :: String -> String
dotPrelude _ = ""

gnuplotPrelude :: String -> String
gnuplotPrelude _ = "set terminal svg;"

-- | The map of all rendering processors.
processors :: Map.Map String Processor
processors =
  Map.fromList
    [ ("dot", Processor [".dot"] "" "")
    , ("gnuplot", Processor [".gnuplot", ".gpi", ".plt", "gp"] "" "")
    , ( "tikz"
      , Processor
          [".tex", ".latex"]
          "\\documentclass{standalone} \\usepackage{tikz} \\usepackage{verbatim} \\begin{document} \\pagestyle{empty}"
          "\\end{document}")
    ]

-- | Calculates the list of all known file extensions that can be rendered into
-- an SVG image.
renderedCodeExtensions :: [String]
renderedCodeExtensions = Map.foldr (\p es -> (extensions p) ++ es) [] processors

-- | Selects a processor based on a list of CSS class names. The first processor
-- that is mentioned in that list is returned.
findProcessor :: [String] -> Maybe Processor
findProcessor classes =
  if renderClass `elem` classes
    then listToMaybe $ Map.elems matching
    else Nothing
  where
    matching = Map.restrictKeys processors (Set.fromList classes)

-- | Appends `.svg` to file urls with extensions that belong to a known render
-- processor. The dependeny for the new file url is established at a later
-- stage, along with the handling of the normal image file urls. 
maybeRenderImage :: Inline -> Inline
maybeRenderImage image@(Image (id, classes, namevals) inlines (url, title)) =
  if takeExtension url `elem` [".dot", ".gnuplot", ".tex"]
    then let svgFile = url <.> "svg"
         in Image (id, classes, namevals) inlines (svgFile, title)
    else image
maybeRenderImage inline = inline

maybeRenderCodeBlock :: Block -> Decker Block
maybeRenderCodeBlock code@(CodeBlock (id, classes, namevals) contents) =
  case findProcessor classes of
    Just processor -> do
      svgFile <- extractCodeIfChanged "" processor contents
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

extractCodeIfChanged :: String -> Processor -> FilePath -> Decker FilePath
extractCodeIfChanged basename processor code = do
  projectDir <- project <$> (lift $ getProjectDirs)
  let crc = printf "%08x" (calc_crc32 code)
  let basepath =
        "generated" </>
        (concat $ intersperse "-" [defaultString "code" basename, crc])
  let extension = (head . extensions) processor
  let sourceFile = projectDir </> basepath <.> extension
  let svgFile = projectDir </> basepath <.> extension <.> ".svg"
  publicResource <- lift $ getPublicResource
  lift $ withResource publicResource 1 $
    liftIO $
    unlessM (System.Directory.doesFileExist svgFile) $ do
      createDirectoryIfMissing True (takeDirectory sourceFile)
      writeFile
        sourceFile
        ((preamble processor) ++ "\n" ++ code ++ "\n" ++ (postamble processor))
  needFile svgFile
  return svgFile

defaultString :: String -> String -> String
defaultString d str
  | null str = d
defaultString _ str = str
