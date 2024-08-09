{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Select (filterSelectedSlides) where

import Relude
import Text.Pandoc (Pandoc (Pandoc), Block)
import Text.Decker.Filter.Monad (Filter)
import Text.Decker.Filter.Slide (fromSlides, toSlides, Slide, header)
import Text.Decker.Internal.Meta (lookupMeta)
import Control.Lens ((^.))
import Text.Pandoc.Definition (Block(Header))

filterSelectedSlides :: Pandoc -> Filter Pandoc
filterSelectedSlides pandoc@(Pandoc meta blocks) = do
    case lookupMeta "lecture.status" meta :: Maybe Text of
        Just "upcoming" -> return $ Pandoc meta (dropSolutionSlides blocks)
        _ -> return pandoc

dropSolutionSlides :: [Block] -> [Block]
dropSolutionSlides blocks = fromSlides $ filter notSolution $ toSlides blocks

notSolution :: Slide -> Bool
notSolution slide =
    case slide ^. header of
        Just (Header 1 (_, cls, _) _) -> "solution" `notElem` cls
        _ -> True
