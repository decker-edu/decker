{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Template (expandTemplateMacros, splice) where

import Data.List qualified as List
import Data.Text qualified as Text
import Relude
import Text.Decker.Filter.Monad (Filter)
import Text.Decker.Filter.Util (randomId)
import Text.Decker.Internal.Meta (lookupMeta)
import Text.Pandoc hiding (lookupMeta, newStdGen)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.RawString.QQ
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- randomId = Text.pack . take 9 . show . md5 . show <$> (randomIO :: IO Int)

expandTemplateMacros :: Pandoc -> Filter Pandoc
expandTemplateMacros (Pandoc meta blocks) = do
  -- Walks link blocks first
  p1 <- liftIO $ walkM expandBlockM blocks
  return $ Pandoc meta (walk expandLink p1)
  where
    -- Expands macro links in block contexts
    expandBlockM (Para [link@(Link attr text (url, title))]) =
      return $ fromMaybe (Para [link]) (expand attr text url title)
    expandBlockM (Plain [link@(Link attr text (url, title))]) =
      return $ fromMaybe (Plain [link]) (expand attr text url title)
    expandBlockM block@(CodeBlock attr code) = do
      id <- randomId
      return $ fromMaybe block (expandCode ("id" <> id) attr code)
    expandBlockM block = return block

    -- Expands macro links in inline contexts
    expandLink link@(Link attr text (url, title)) =
      fromMaybe link (expand attr text url title)
    expandLink inline = inline

    expandCode rndId attr@(id, cls, kvs) code = do
      let rawKvs = map (\(k, v) -> (fromMaybe k $ Text.stripPrefix "data-" k, v)) kvs
      name <- List.lookup "macro" rawKvs
      let kvAttribs = List.filter ((/= "macro") . fst) rawKvs
      let clsArgs = zip (map show [1 .. (length cls)]) cls
      let allClsArgs = [("args", Text.unwords cls)]
      let allKvAttribs = [("attribs", unwords $ map (\(k, v) -> k <> "=\"" <> v <> "\"") kvAttribs)]
      let codeArg = [("code", Text.strip code)]
      let rndIdArg = [("rnd-id", rndId)]
      let idArg = [("id", if Text.null id then rndId else id)]
      let captionArg = [("caption", fromMaybe "" (List.lookup "caption" rawKvs))]
      let arguments = codeArg <> clsArgs <> allClsArgs <> kvAttribs <> allKvAttribs <> rndIdArg <> idArg <> captionArg
      template <- lookupMeta ("templates." <> name) meta
      let result =
            case template of
              MetaInlines inlines -> splice name $ walk (substituteInline arguments) blocks
              MetaBlocks [Plain blocks] -> splice name $ walk (substituteInline arguments) blocks
              MetaBlocks [Para blocks] -> splice name $ walk (substituteInline arguments) blocks
              MetaBlocks blocks -> splice name $ walk (substituteInline arguments) $ walk (substituteBlock arguments) blocks
              MetaString str -> splice name $ substitute arguments str
              _ -> splice name $ CodeBlock attr code
      Just result

    -- Expand macro and splice back into required context
    expand attr text url title = do
      (name, args) <- parseInvocation text
      let targetArgs = [("url", url), ("title", title)]
      let posArgs = zip (map show [1 .. (length args)]) args
      let allPosArgs = [("args", Text.unwords args)]
      let arguments = allPosArgs <> posArgs <> targetArgs
      template <- lookupMeta ("templates." <> name) meta
      Just $ case template of
        MetaInlines inlines -> splice name $ walk (substituteInline arguments) inlines
        MetaBlocks [Plain blocks] -> splice name $ walk (substituteInline arguments) blocks
        MetaBlocks [Para blocks] -> splice name $ walk (substituteInline arguments) blocks
        MetaBlocks blocks -> splice name $ walk (substituteInline arguments) $ walk (substituteBlock arguments) blocks
        MetaString str -> splice name $ substitute arguments str
        _ -> splice name $ Link attr text (url, title)

    -- Parses a link text into a macro invocation, if possible. a macro name
    -- starts either with  a '@', or end with a ':'
    parseInvocation inline =
      case second Text.words $ Text.splitAt 1 $ stringify inline of
        ("@", name : args) -> Just (name, args)
        _ -> case Text.words $ stringify inline of
          (name : args) | Text.isSuffixOf ":" name -> Just (Text.dropEnd 1 name, args)
          _ -> Nothing

    -- Substitutes macro arguments into text fragments in various Inline elements
    substituteInline args (Str text) = Str (substitute args text)
    substituteInline args (Code attr text) = Code (substituteAttr args attr) (substitute args text)
    substituteInline args (Span attr inlines) =
      Span (substituteAttr args attr) inlines
    substituteInline args (Link attr text (url, title)) =
      Link (substituteAttr args attr) text (substitute args url, substitute args title)
    substituteInline args (Image attr text (url, title)) =
      Image (substituteAttr args attr) text (substitute args url, substitute args title)
    substituteInline args (RawInline "html" html) =
      RawInline "html" (substitute args html)
    substituteInline args inline = inline

    -- Substitutes macro arguments into text fragments in RawBlock elements.
    -- Substitution in CodeBlocks is probably not a good idea.
    substituteBlock args (Div attr blocks) =
      Div (substituteAttr args attr) blocks
    substituteBlock args (Header level attr inline) =
      Header level (substituteAttr args attr) inline
    substituteBlock args (RawBlock "html" html) =
      RawBlock "html" (substitute args html)
    substituteBlock args block = block

    substituteAttr args (id, cls, kvs) =
      (substitute args id, map (substitute args) cls, map (bimap (substitute args) (substitute args)) kvs)

    -- substitute args text = foldl' substituteOne text args

    -- substitutes all macro invocations with the value bound to the provided
    -- attribute. if none is found, the invocation is left in the text
    substitute :: [(Text, Text)] -> Text -> Text
    substitute args intext =
      let (before, _, after, groups) :: (Text, Text, Text, [Text]) =
            intext =~ ([r|:\(([a-z][-|_a-z0-9]*)\)|] :: Text)
       in case viaNonEmpty head groups >>= findValue args of
            Just value -> substitute args $ Text.intercalate value [before, after]
            Nothing -> intext

    -- finds the value for the first agrument name that is bound in `args`. if
    -- there is none, the name of the last argument is used verbatim.
    findValue :: [(Text, Text)] -> Text -> Maybe Text
    findValue args param =
      let params = Text.splitOn "|" param
          fallback = viaNonEmpty last params
       in asum $ map (`List.lookup` args) params <> [fallback]

-- substituteOne template (name, value) =
--   Text.intercalate value
--     $ Text.splitOn (":(" <> name <> ")") template

class Splice a b where
  splice :: Text -> a -> b

instance Splice Inline Inline where
  splice macro = id

instance Splice Text Inline where
  splice macro = Str

instance Splice Text [Inline] where
  splice macro text = [Str text]

instance Splice Text Block where
  splice macro text = Para [Str text]

instance Splice [Inline] [Inline] where
  splice macro = id

instance Splice [Inline] Inline where
  splice macro = Span nullAttr

instance Splice Block Block where
  splice macro = id

instance Splice [Block] Block where
  splice macro = Div nullAttr

instance Splice Inline Block where
  splice macro block = Para [block]

instance Splice [Inline] Block where
  splice macro = Para

instance Splice Block Inline where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice Block into Inline context"

instance Splice Block [Inline] where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice Block into [Inline] context"

instance Splice [Block] Inline where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice [Block] into Inline context"

instance Splice [Block] [Inline] where
  splice macro inline = error $ "template '" <> macro <> "': cannot splice [Block] into [Inline] context"
