{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Render
  ( renderCodeBlocks
  , renderedCodeExtensions
  , appendScripts
  ) where

import CRC32
import Common
import Context
import Control.Monad.State
import Control.Monad.Extra
import Data.List
import Data.List.Extra
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Project
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H ((!), canvas, div, script, toValue)
import Text.Blaze.Html5.Attributes as A (class_, id, lang, src)
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
processors :: Map.Map String Processor
processors =
  Map.fromList
    [ ("dot", Processor ".dot" (shakeCompile ".svg"))
    , ("gnuplot", Processor ".gnuplot" (shakeCompile ".svg"))
    , ("tikz", Processor ".tex" (bracketedShakeCompile ".svg" tikzPre tikzPost))
    , ("d3", Processor ".js" d3Canvas)
    , ("threejs", Processor ".js" threejsCanvas)
    ]

tikzPre :: String
tikzPre =
  "\\documentclass{standalone} \\usepackage{tikz} \\usepackage{verbatim}\n" ++
  "\\begin{document} \\pagestyle{empty}"

tikzPost :: String
tikzPost = "\\end{document}"

d3Canvas :: FilePath -> Attr -> Decker Inline
d3Canvas source (eid, classes, keyvals) = do
  needFile source
  -- TODO: Clean this up. See Path.hs.
  base <- gets basePath
  dirs <- lift $ getProjectDirs
  let publicBase = public dirs </> makeRelativeTo (project dirs) base
  supportDir <- lift $ getRelativeSupportDir publicBase
  contents <- doIO $ readFile source
  addScript $ ScriptURI "javascript" (supportDir </> "d3.js")
  addScript $ ScriptSource "javascript" contents
  let classStr = intercalate " " classes
  let element = fromMaybe "svg" $ lookup "element" keyvals
  case element of
    "canvas" ->
      return $
      RawInline (Format "html") $
      renderHtml $
      H.canvas ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    "div" ->
      return $
      RawInline (Format "html") $
      renderHtml $ H.div ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    _ ->
      return $
      RawInline (Format "html") $
      printf "<svg id=\"%v\" class=\"%v\"></svg>" eid classStr

threejsCanvas :: FilePath -> Attr -> Decker Inline
threejsCanvas source (eid, classes, keyvals) = do
  needFile source
  -- TODO: Clean this up. See Path.hs.
  base <- gets basePath
  dirs <- lift $ getProjectDirs
  let publicBase = public dirs </> makeRelativeTo (project dirs) base
  supportDir <- lift $ getRelativeSupportDir publicBase
  contents <- doIO $ readFile source
  addScript $ ScriptURI "javascript" (supportDir </> "three.js")
  let includes = splitOn "," $ fromMaybe "" $ lookup "includes" keyvals
  mapM_ addScript $ map (ScriptURI "javascript") includes
  addScript $ ScriptSource "javascript" contents
  let classStr = intercalate " " classes
  let element = fromMaybe "svg" $ lookup "element" keyvals
  case element of
    "canvas" ->
      return $
      RawInline (Format "html") $
      renderHtml $
      H.canvas ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    "div" ->
      return $
      RawInline (Format "html") $
      renderHtml $ H.div ! A.id (toValue eid) ! A.class_ (toValue classStr) $ ""
    _ ->
      return $
      RawInline (Format "html") $
      printf "<svg id=\"%v\" class=\"%v\"></svg>" eid classStr

bracketedShakeCompile ::
     String -> String -> String -> FilePath -> Attr -> Decker Inline
bracketedShakeCompile ext preamble postamble source attr = do
  contents <- doIO $ readFile source
  let bracketed = preamble ++ "\n" ++ contents ++ "\n" ++ postamble
  codePath <- writeCodeIfChanged bracketed (takeExtension source)
  let path = codePath <.> ext
  needFile path
  return $ Image attr [] (path, "")

shakeCompile :: String -> FilePath -> Attr -> Decker Inline
shakeCompile ext source attr = do
  let path = source <.> ext
  needFile path
  return $ Image attr [] (path, "")

-- | Calculates the list of all known file extensions that can be rendered into
-- an SVG image.
renderedCodeExtensions :: [String]
renderedCodeExtensions = [".dot", ".gnuplot", ".tex", ".js"]

restrictKeys :: Ord a1 => Map.Map a1 a -> Set.Set a1 -> Map.Map a1 a
restrictKeys m s = Map.filterWithKey (\k _ -> k `Set.member` s) m

-- | Selects a processor based on a list of CSS class names. The first processor
-- that is mentioned in that list is returned.
findProcessor :: [String] -> Maybe Processor
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
    Just processor -> do
      (compiler processor) url attr
    Nothing -> return image
maybeRenderImage inline = return inline

maybeRenderCodeBlock :: Block -> Decker Block
maybeRenderCodeBlock block@(CodeBlock attr@(_, classes, _) code) =
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
-- svgDataUrl :: String -> String
-- svgDataUrl svg =
--   "data:image/svg+xml;base64," ++ (B.unpack (B64.encode (B.pack svg)))
writeCodeIfChanged :: String -> String -> Decker FilePath
writeCodeIfChanged code ext = do
  projectDir <- project <$> (lift $ getProjectDirs)
  let crc = printf "%08x" (calc_crc32 code)
  let basepath = "code" </> (concat $ intersperse "-" ["code", crc])
  let path = projectDir </> basepath <.> ext
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
    renderScript (ScriptURI language uri) =
      RawBlock (Format "html") $
      renderHtml $
      H.script ! class_ "generated decker" ! lang (toValue language) !
      src (toValue uri) $
      ""
    renderScript (ScriptSource language source) = do
      RawBlock (Format "html") $
        printf
          "<script class=\"generated decker\" lang=\"%s\">%s</script>"
          language
          source
      -- renderHtml $
      -- H.script ! class_ "generated decker" ! lang = language $ preEscapedToHtml source
