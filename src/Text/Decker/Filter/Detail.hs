{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Detail where

import qualified Data.List as List
import Data.List.Split
import Relude
import Text.Decker.Filter.Attrib (addClasses)
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Pandoc.Walk

processDetailDiv :: Pandoc -> Decker Pandoc
processDetailDiv (Pandoc meta blocks) = return $ Pandoc meta (walk makeDivDetail blocks)
  where
    makeDivDetail (Div attr@(_, cls, kvs) body)
      | "details" `elem` cls =
        let summary = maybe [] ((: []) . Str) $ List.lookup "summary" kvs
         in makeDetail attr summary body
    makeDivDetail div = div

makeDetail :: Attr -> [Inline] -> [Block] -> Block
makeDetail attr summary detail =
  let sum = case summary of
        [] -> []
        text -> [tag "summary" $ Div nullAttr [Plain text]]
   in tag "details" $
        Div
          (addClasses ["block"] attr)
          (sum <> detail)

makeFramedDetail :: Attr -> [Inline] -> [Block] -> Block
makeFramedDetail attr summary detail =
  let sum = case summary of
        [] -> []
        text -> [tag "summary" $ Div nullAttr [Plain text]]
   in Div ("", ["details-frame"], []) 
        [tag "details" $
            Div
                (addClasses ["block"] attr)
                (sum <> detail)]

processDetailHeader :: Pandoc -> Decker Pandoc
processDetailHeader (Pandoc meta blocks) =
  return $ Pandoc meta $ processDetailHeader' 1 blocks

processDetailHeader' :: Int -> [Block] -> [Block]
processDetailHeader' level blocks = concatMap process sections
  where
    sections = split (keepDelimsL $ whenElt (isBlockStart level)) blocks
    process (header : blocks) =
      transformDetail (header : processDetailHeader' (level + 1) blocks)
    process [] = []
    isBlockStart level (Header n attr text) | n <= level = True
    isBlockStart _ _ = False
    transformDetail ((Header n attr@(id, cls, kvs) text) : rest)
      | "details" `elem` cls = [makeFramedDetail attr text rest]
    transformDetail blocks = blocks
