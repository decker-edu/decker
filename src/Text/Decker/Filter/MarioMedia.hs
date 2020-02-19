module Text.Decker.Filter.MarioMedia
  ( marioMedia
  ) where

import Control.Exception
import Data.Char (toLower)
import Data.String (IsString)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath
import Text.Pandoc.JSON
import Text.Pandoc.Walk

import Text.Decker.Filter.Util
import Text.Decker.Internal.Common

-- | File-extensions that should be treated as audio
audioExt :: [String]
audioExt = [".mp3", ".aac"]

-- | File-extensions that should be treated as video
videoExt :: [String]
videoExt = [".avi", ".mp4", ".mov"]

-- | File-extensions that should be treated as image
imgExt :: [String]
imgExt = [".jpg", ".jpeg", ".png", ".gif", ".tif", ".tiff", ".bmp", ".svg"]

-- | File-extensions that should be treated as PDF
pdfExt :: [String]
pdfExt = [".pdf"]

-- | File-extensions that should be treated as demo and will be included
-- in an iframe
demoExt :: [String]
demoExt = [".html", ".htm", ".php"]

-- | File-extensions that should be treated as 3D model and will be shown with Mario's viewer
-- in an iframe
meshExt :: [String]
meshExt = [".off", ".obj", ".stl", ".pmp", ".ply", ".xyz"]

-- | main media-plugin.
--
-- Will convert the following syntax
--
-- @
-- ![](foo.aac){#audio}
-- ![](foo.mp4){#video}
-- ![](foo.png){#img}
-- ![](foo.svg){#svg}
-- ![](foo.html){#demo}
-- @
--
-- HTML-id's maybe ommitted if the file-extension is in whitelist.
--
-- If a type is detected by extension a custom id (not matching the extension)
-- will be preserved.
--
media :: Inline -> IO [Inline]
--audio
media (Image (id', att, att') [] (filename, _))
  | id' == "audio" || checkExtension filename audioExt =
    return $
    [ toHtml $
      "<audio " <> Text.unwords direct <> " src=\"" <> filename <> "\"" <>
      attToString (idFilter "audio" id', css, att') <>
      "></audio>"
    ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
media (Image (id', att, att') alt (filename, _))
  | id' == "audio" || checkExtension filename audioExt =
    return $
    [ toHtml $
      "<figure " <> attToString (idFilter "audio" id', css, att') <> "><audio " <>
      Text.unwords direct <>
      " src=\"" <>
      filename <>
      "\"></audio>"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
--videos
media (Image (id', att, att') [] (filename, _))
  | id' == "video" || checkExtension filename videoExt =
    return $
    [ toHtml $
      "<video " <> Text.unwords direct <> " data-src=\"" <> filename <> "\"" <>
      attToString (idFilter "video" id', css, att') <>
      "></video>"
    ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
media (Image (id', att, att') alt (filename, _))
  | id' == "video" || checkExtension filename videoExt =
    return $
    [ toHtml $
      "<figure " <> attToString (idFilter "video" id', css, att') <> ">"
    ] <>
    [ toHtml $
      "<video " <> Text.unwords direct <> " data-src=\"" <> filename <>
      "\" style=\"" <>
      style <>
      "\"></video>"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
    style = filterStyle att'
--load svg and dump it in
media (Image (id', att, att') [] (filename, _))
  | id' == "svg" =
    handle
      (\(fileerror :: IOException) ->
         return
           [ toHtml $
             "Could not read file: " <> filename <> "<br />" <>
             (Text.pack . show) fileerror
           ]) $ do
      svg <- Text.readFile (Text.unpack filename)
      return $
        [ toHtml $
          "<figure " <> Text.unwords direct <> " " <>
          attToString (idFilter "svg" id', css, att') <>
          ">"
        ] -- use attributes on figure, as svg gets dumped in..
         <>
        [toHtml $ svg] <>
        [toHtml $ "</figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
media (Image (id', att, att') alt (filename, _))
  | id' == "svg" =
    handle
      (\(fileerror :: IOException) ->
         return
           [ toHtml $
             "Could not read file: " <> filename <> "<br />" <>
             (Text.pack . show) fileerror
           ]) $ do
      svg <- Text.readFile (Text.unpack filename)
      return $
        [ toHtml $
          "<figure " <> Text.unwords direct <> " " <>
          attToString (idFilter "svg" id', css, att') <>
          ">"
        ] -- use attributes on figure, as svg gets dumped in..
         <>
        [toHtml $ svg] <>
        [toHtml $ "<figcaption>"] <>
        alt <>
        [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
--images
media (Image (id', att, att') [] (filename, _))
  | id' == "img" || checkExtension filename imgExt =
    return $
    [ toHtml $
      "<img " <> attToString (idFilter "img" id', css, att') <>
      Text.unwords direct <>
      " src=\"" <>
      filename <>
      "\" style=\"" <>
      style <>
      "\"></img>"
    ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
    style = filterStyle att'
media (Image (id', att, att') alt (filename, _))
  | id' == "img" || checkExtension filename imgExt =
    return $
    [toHtml $ "<figure " <> attToString (idFilter "img" id', css, att') <> ">"] <>
    [ toHtml $
      "<img " <> Text.unwords direct <> " src=\"" <> filename <> "\" style=\"" <>
      style <>
      "\"></img>"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
    style = filterStyle att'
--PDF
media (Image (id', att, att') [] (filename, _))
  | id' == "pdf" || checkExtension filename pdfExt =
    return $
    [ toHtml $
      "<embed " <> attToString (idFilter "pdf" id', css, att') <>
      Text.unwords direct <>
      " src=\"" <>
      filename <>
      "\" style=\"" <>
      style <>
      "\">"
    ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
    style = filterStyle att'
media (Image (id', att, att') alt (filename, _))
  | id' == "pdf" || checkExtension filename imgExt =
    return $
    [toHtml $ "<figure " <> attToString (idFilter "pdf" id', css, att') <> ">"] <>
    [ toHtml $
      "<embed " <> Text.unwords direct <> " src=\"" <> filename <> "\" style=\"" <>
      style <>
      "\">"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
    style = filterStyle att'
--html-demos etc. as IFrames (use data-src instead of src to enable lazy-loading)
media (Image (id', att, att') [] (filename, _))
  | id' == "demo" || checkExtension filename demoExt =
    return
      [ toHtml $
        "<iframe allow=\"fullscreen\"" <> Text.unwords direct <> " data-src=\"" <>
        filename <>
        "\" " <>
        attToString (idFilter "demo" id', css, att') <>
        "></iframe>"
      ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
media (Image (id', att, att') alt (filename, _))
  | id' == "demo" || checkExtension filename demoExt =
    return $
    [toHtml $ "<figure " <> attToString (idFilter "demo" id', css, att') <> ">"] <>
    [ toHtml $
      "<iframe allow=\"fullscreen\"" <> Text.unwords direct <> " data-src=\"" <>
      filename <>
      "\"" <>
      attToString (idFilter "demo" id', css, att') <>
      " ></iframe>"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
--3D meshes shown in a WebGL viewer in an iframe (use data-src to enable reveal's lazy loading)
media (Image (id', att, att') [] (filename, _))
  | checkExtension filename meshExt =
    return
      [ toHtml $
        "<iframe allow=\"fullscreen\" data-src=\"support/mview/mview.html\" model=\"" <>
        filename <>
        "\" " <>
        Text.unwords direct <>
        " " <>
        attToString (id', css, att') <>
        "></iframe>"
      ]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
media (Image (id', att, att') alt (filename, _))
  | checkExtension filename meshExt =
    return $
    [toHtml $ "<figure " <> attToString (id', css, att') <> ">"] <>
    [ toHtml $
      "<iframe allow=\"fullscreen\" data-src=\"support/mview/mview.html\" model=\"" <>
      filename <>
      "\" " <>
      Text.unwords direct <>
      " " <>
      attToString (id', css, att') <>
      "></iframe>"
    ] <>
    [toHtml $ "<figcaption>"] <>
    alt <>
    [toHtml $ "</figcaption></figure>"]
  where
    (direct, css) = (classToRevealAttr . revealjsRewriteAttr) att
-- if not matched
media x = return [x]

-- return filename extension (strip additional arguments from HTML URLs)
checkExtension :: Text.Text -> [String] -> Bool
checkExtension fn exts =
  (fmap toLower . takeExtension . takeWhile (/= '?') . takeWhile (/= '#') . Text.unpack) fn `elem`
  exts

-- checkExtension fn exts = (fmap toLower . takeExtension) fn `elem` exts
idFilter :: (IsString a, Eq a) => a -> a -> a
idFilter a b
  | a == b = ""
  | otherwise = b

filterStyle :: [(Text.Text, Text.Text)] -> Text.Text
filterStyle kvpairs =
  case filter ((== "style") . fst) (convertToStyle ["width", "height"] kvpairs) of
    [] -> ""
    as -> Text.intercalate ";" $ snd <$> as

marioMedia :: Pandoc -> Decker Pandoc
marioMedia = doIO . walkM media'
  where
    media' :: [Inline] -> IO [Inline]
    media' inlines = concat <$> mapM media inlines
