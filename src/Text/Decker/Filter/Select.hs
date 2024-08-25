{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Select (filterSelectedSlides, dropSolutionBlocks) where

import Relude
import Text.Pandoc (Pandoc (Pandoc), Block)
import Text.Decker.Filter.Monad (Filter)
import Text.Decker.Filter.Slide (fromSlides, toSlides, Slide (Slide), header)
import Text.Decker.Internal.Meta (lookupMeta)
import Control.Lens ((^.))
import Text.Pandoc.Definition (Block(Header), Meta)
import Text.Pandoc (Block(Div))
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
