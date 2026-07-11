{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Select (filterSelectedSlides, dropSolutionBlocks, dropSolutionContent, publishingUpcoming) where

import Relude
import Data.List.Split (keepDelimsL, split, whenElt)
import Text.Pandoc ( Pandoc(Pandoc), Block, Block(Div) )
import Text.Decker.Filter.Monad (Filter)
import Text.Decker.Filter.Slide (fromSlides, toSlides, Slide (Slide), header, isBoxDelim)
import Text.Decker.Internal.Meta (lookupMeta)
import Control.Lens ((^.))
import Text.Pandoc.Definition (Block(Header), Meta)
import Text.Decker.Internal.Common (Decker)

filterSelectedSlides :: Pandoc -> Filter Pandoc
filterSelectedSlides pandoc@(Pandoc meta blocks) =
    return $ if publishingUpcoming meta
        then Pandoc meta (dropSolutionSlides blocks)
        else pandoc

publishingUpcoming meta =
    (lookupMeta "lecture.publish" meta :: Maybe Text) == Just "yes" &&
    (lookupMeta "lecture.status" meta :: Maybe Text) == Just "upcoming"

dropSolutionSlides :: [Block] -> [Block]
dropSolutionSlides blocks = fromSlides $ filter notSolution $ toSlides blocks

dropSolutionBlocks  :: Meta -> Slide -> Decker Slide
dropSolutionBlocks meta slide@(Slide header body dir) =
    return $ if publishingUpcoming meta
        then Slide header (filter notSolutionBlock body) dir
        else slide
    where
        notSolutionBlock (Div (_,cls,_) body) | "box" `elem` cls = "solution" `notElem` cls
        notSolutionBlock _ = True

notSolution :: Slide -> Bool
notSolution slide =
    case slide ^. header of
        Just (Header 1 (_, cls, _) _) -> "solution" `notElem` cls
        _ -> True

-- | Pure publishing filter for the chatty/markdown output. When an upcoming
-- lecture is being published (see 'publishingUpcoming'), drops solution slides
-- (level-1 headers with class @solution@) and solution boxes (the level-2
-- header carrying class @solution@ together with all of its content up to the
-- next box). This mirrors the filtering applied to the HTML slide output, but
-- operates on the raw source blocks so the result is still clean Markdown. The
-- decision is taken on @decisionMeta@ (the merged global+document meta) while
-- the document's own meta is preserved in the result.
dropSolutionContent :: Meta -> Pandoc -> Pandoc
dropSolutionContent decisionMeta pandoc@(Pandoc meta blocks)
    | publishingUpcoming decisionMeta =
        Pandoc meta (fromSlides $ map dropSolutionBoxes $ filter notSolution $ toSlides blocks)
    | otherwise = pandoc

dropSolutionBoxes :: Slide -> Slide
dropSolutionBoxes (Slide hdr body dir) =
    Slide hdr (concat $ filter (not . isSolutionBox) $ split (keepDelimsL $ whenElt isBoxDelim) body) dir
    where
        isSolutionBox (Header 2 (_, cls, _) _ : _) = "solution" `elem` cls
        isSolutionBox _ = False
