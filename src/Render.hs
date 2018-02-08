{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Render
  ( renderCodeBlocks
  , renderedCodeExtensions
  , appendScripts
  ) where

import CRC32
import Common
import Context
import Control.Monad.State
import Control.Monad.Trans.Class
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Extra
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Development.Shake
import Extra
import Network.URI
import Project
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
       ((!), canvas, div, preEscapedToHtml, script, toHtml, toValue)
import Text.Blaze.Html5.Attributes as A
       (alt, class_, height, id, src, style, title, width)
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
    [ ("dot", Processor ".dot" (shakeCompile ".svg"))
    , ("gnuplot", Processor ".gnuplot" (shakeCompile ".svg"))
    , ("tikz", Processor ".tex" (bracketedShakeCompile ".svg" tikzPre tikzPost))
    , ("d3", Processor ".js" d3Canvas)
    ]

tikzPre =
  "\\documentclass{standalone} \\usepackage{tikz} \\usepackage{verbatim}\n" ++
  "\\begin{document} \\pagestyle{empty}"

tikzPost = "\\end{document}"

d3Canvas :: FilePath -> Attr -> Decker Inline
d3Canvas sourceFile (eid, classes, keyvals) = do
  needFile sourceFile
  base <- gets basePath
  supportDir <- lift $ getRelativeSupportDir base
  source <- doIO $ readFile sourceFile
  addScript $
    ScriptURI "javascript" (supportDir </> "d3.v4.min.js")
  addScript $ ScriptSource "javascript" source
  let element = fromMaybe "svg" $ lookup "element" keyvals
  let classStr = intercalate " " classes
  case element of
    "canvas" ->
      return $
      RawInline (Format "html") $
      renderHtml $
      H.canvas ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    "div" ->
      return $
      RawInline (Format "html") $
      renderHtml $
      H.div ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    _ ->
      return $
      RawInline (Format "html") $
      printf "<svg id=\"%v\" class=\"%v\"></svg>" eid classStr

bracketedShakeCompile ::
     String -> String -> String -> FilePath -> Attr -> Decker Inline
bracketedShakeCompile extension preamble postamble sourceFile attr = do
  source <- doIO $ readFile sourceFile
  let bracketed = preamble ++ "\n" ++ source ++ "\n" ++ postamble
  codePath <- writeCodeIfChanged bracketed (takeExtension sourceFile)
  let path = codePath <.> extension
  needFile path
  return $ Image attr [] (path, "")

shakeCompile :: String -> FilePath -> Attr -> Decker Inline
shakeCompile extension sourceFile attr = do
  let path = sourceFile <.> extension
  needFile path
  return $ Image attr [] (path, "")

-- | Calculates the list of all known file extensions that can be rendered into
-- an SVG image.
renderedCodeExtensions :: [String]
renderedCodeExtensions = [".dot", ".gnuplot", ".tex", ".js"]

-- | Selects a processor based on a list of CSS class names. The first processor
-- that is mentioned in that list is returned.
findProcessor :: [String] -> Maybe Processor
findProcessor classes
  | "render" `elem` classes = listToMaybe $ Map.elems matching
  where
    matching = Map.restrictKeys processors (Set.fromList classes)
findProcessor _ = Nothing

-- | Appends `.svg` to file urls with extensions that belong to a known render
-- processor. The dependeny for the new file url is established at a later
-- stage, along with the handling of the normal image file urls. TODO: Fetch and
-- cache remote URLs here. For now, assume local urls.
maybeRenderImage :: Inline -> Decker Inline
maybeRenderImage image@(Image attr@(id, classes, namevals) inlines (url, title)) =
  case findProcessor classes of
    Just processor -> do
      (compiler processor) url attr
    Nothing -> return image
maybeRenderImage inline = return inline

maybeRenderCodeBlock :: Block -> Decker Block
maybeRenderCodeBlock block@(CodeBlock attr@(eid, classes, namevals) code) =
  case findProcessor classes of
    Just processor -> do
      path <- writeCodeIfChanged code (extension processor)
      inline <- (compiler processor) path attr
      return $ Plain [inline]
    Nothing -> return block
maybeRenderCodeBlock block = return block

{--
provideResources namevals = do
  case lookup "resources" namevals of
    Just resources -> do
      method <- gets provisioning
--}

-- | Encode a svg snippet into a data url for an image element
svgDataUrl :: String -> String
svgDataUrl svg =
  "data:image/svg+xml;base64," ++ (B.unpack (B64.encode (B.pack svg)))

writeCodeIfChanged :: String -> String -> Decker FilePath
writeCodeIfChanged code extension = do
  projectDir <- project <$> (lift $ getProjectDirs)
  let crc = printf "%08x" (calc_crc32 code)
  let basepath = "code" </> (concat $ intersperse "-" ["code", crc])
  let path = projectDir </> basepath <.> extension
  lift $
    withShakeLock $
    liftIO $
    unlessM (System.Directory.doesFileExist path) $ do
      createDirectoryIfMissing True (takeDirectory path)
      writeFile path code
  return path

appendScripts :: Pandoc -> Decker Pandoc
appendScripts pandoc@(Pandoc meta blocks) = do
  disp <- gets disposition
  case disp of
    (Disposition _ Html) -> do
      collected <- nubOrd <$> gets scripts
      return $ Pandoc meta (blocks ++ map renderScript collected)
    (Disposition _ _) -> return pandoc
  where
    renderScript (ScriptURI lang uri) =
      RawBlock (Format "html") $
      renderHtml $
      H.script ! class_ "generated decker" ! src (toValue uri) $ ""
    renderScript (ScriptSource lang source) = do
      RawBlock (Format "html") $
        printf "<script class=\"generated decker\">%s</script>" source
      -- renderHtml $
      -- H.script ! class_ "generated decker" $ preEscapedToHtml source
