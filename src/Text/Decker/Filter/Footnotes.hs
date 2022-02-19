{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Footnotes
  ( renderFootnotes,
  )
where

import Data.Maybe
import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc

renderFootnotes :: Slide -> Decker Slide
renderFootnotes slide@(Slide (Just header) body dir) = do
  let (strippedHeader, headerNotes) = collect [header]
  let (strippedBody, bodyNotes) = collect body
  return
    ( Slide
        (listToMaybe strippedHeader)
        (strippedBody <> render (headerNotes <> bodyNotes))
        dir
    )
renderFootnotes slide@(Slide Nothing body dir) = do
  let (strippedBody, bodyNotes) = collect body
  return (Slide Nothing (strippedBody <> render bodyNotes) dir)

collect :: [Block] -> ([Block], [Inline])
collect blocks = ([], [])

render :: [Inline] -> [Block]
render = map render'
  where
    render' (Note blocks) = Div ("", ["footnote"], []) blocks
    render' _ = error "Nothing but `Note` allowed here."