{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Footnotes
  ( renderFootnotes,
  )
where

import Relude
import Text.Pandoc
import Text.Pandoc.Walk

-- |  Collects footnotes on a slide and append them to the end of the slide.
renderFootnotes :: [Block] -> [Block]
renderFootnotes blocks =
  let (Div _ noted, (_, notes)) = runState (walkM scan (Div nullAttr blocks)) (0, [])
      footnotes = Div ("", ["footnotes"], []) [OrderedList (1, Decimal, OneParen) notes]
   in noted <> [footnotes]
  where
    scan note@(Note blocks) = do
      (n, notes) <- get
      put (n + 1, notes <> [blocks])
      return $ Span ("", ["footnoteref"], []) [Str (show (n + 1))]
    scan inline = return inline
