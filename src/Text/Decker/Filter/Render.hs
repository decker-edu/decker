{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Filter.Render
  ( renderCodeBlocks
  , renderedCodeExtensions
  ) where

import Text.Decker.Filter.CRC32
import Text.Decker.Internal.Common
import Text.Decker.Project.Project
import Text.Decker.Project.Shake

import Control.Monad.Extra
import Control.Monad.State
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Walk
import Text.Printf

-- | Evaluate code blocks
renderCodeBlocks :: Pandoc -> Decker Pandoc
renderCodeBlocks pandoc =
  walkM maybeRenderImage pandoc >>= walkM maybeRenderCodeBlock

data Processor = Processor
  { extension :: String
  , compiler :: String -> Attr -> Decker Inline
  }

-- | The map of all rendering processors.
processors :: Map.Map Text.Text Processor
processors =
  Map.fromList
    [ ("dot", Processor ".dot" (shakeCompile ".svg"))
    , ("gnuplot", Processor ".gnuplot" (shakeCompile ".svg"))
    , ("tikz", Processor ".tex" (bracketedShakeCompile ".svg" tikzPre tikzPost))
    ]

tikzPre :: String
tikzPre =
  "\\documentclass{standalone} \\usepackage{tikz} \\usepackage{verbatim}\n" ++
  "\\begin{document} \\pagestyle{empty}"

tikzPost :: String
tikzPost = "\\end{document}"

bracketedShakeCompile ext preamble postamble source attr = do
  contents <- doIO $ readFile source
  let bracketed = preamble ++ "\n" ++ contents ++ "\n" ++ postamble
  codePath <- writeCodeIfChanged bracketed (takeExtension source)
  let path = codePath <.> ext
  needFile path
  return $ Image attr [] (Text.pack path, "")

shakeCompile :: String -> FilePath -> Attr -> Decker Inline
shakeCompile ext source attr = do
  let path = source <.> ext
  needFile path
  return $ Image attr [] (Text.pack path, "")

-- | Calculates the list of all known file extensions that can be rendered into
-- an SVG image.
renderedCodeExtensions :: [String]
renderedCodeExtensions = [".dot", ".gnuplot", ".tex"]

restrictKeys :: Ord a1 => Map.Map a1 a -> Set.Set a1 -> Map.Map a1 a
restrictKeys m s = Map.filterWithKey (\k _ -> k `Set.member` s) m

-- | Selects a processor based on a list of CSS class names. The first processor
-- that is mentioned in that list is returned.
findProcessor :: [Text.Text] -> Maybe Processor
findProcessor classes
  | "render" `elem` classes = listToMaybe $ Map.elems matching
  where
    matching = restrictKeys processors (Set.fromList classes)
findProcessor _ = Nothing

-- | Appends `.svg` to file urls with extensions that belong to a known render
-- processor. The dependeny for the new file url is established at a later
-- stage, along with the handling of the normal image file urls. TODO: Fetch and
-- cache remote URLs here. For now, assume local urls.
maybeRenderImage :: Inline -> Decker Inline
maybeRenderImage image@(Image attr@(_, classes, _) _ (url, _)) =
  case findProcessor classes of
    Just processor -> compiler processor (Text.unpack url) attr
    Nothing -> return image
maybeRenderImage inline = return inline

maybeRenderCodeBlock :: Block -> Decker Block
maybeRenderCodeBlock block@(CodeBlock attr@(x, cls, y) code) =
  case findProcessor cls of
    Just processor -> do
      path <- writeCodeIfChanged (Text.unpack code) (extension processor)
      inline <- compiler processor path (x, cls, y)
      return $ Plain [inline]
    Nothing -> return block
maybeRenderCodeBlock block = return block

writeCodeIfChanged :: String -> String -> Decker FilePath
writeCodeIfChanged code ext = do
  projectDir <- _project <$> lift projectDirsA
  let crc = printf "%08x" (calc_crc32 code)
  -- TODO get rid of the deckerFiles reference once the Decker monad is removed
  let basepath = deckerFiles </> "code" </> intercalate "-" ["code", crc]
  let path = projectDir </> basepath <.> ext
  lift $
    withShakeLock $
    liftIO $
    unlessM (System.Directory.doesFileExist path) $ do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path code
  return path
