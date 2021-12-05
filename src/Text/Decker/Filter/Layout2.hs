{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Layout2
  ( layoutSlide2,
  )
where

import Control.Lens
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common hiding (Layout)
import Text.Pandoc hiding (Row)
import Text.Pandoc.Definition ()
import Text.Pandoc.Lens
import Text.Regex.TDFA

-- | Slide layouts are rows of one ore more columns.
data Layout
  = RowLayout
      { name :: Text,
        rows :: [Row]
      }
  | GridLayout
      { name :: Text,
        areas :: [Text]
      }
  deriving (Eq, Show)

-- | A row consists of one or more columns.
data Row
  = SingleColumn Text
  | MultiColumn [Text]
  deriving (Eq, Show)

type Area = [Block]

type AreaMap = [(Text, Area)]

layouts :: [Layout]
layouts =
  [ RowLayout
      "columns"
      [ SingleColumn "top",
        MultiColumn ["left", "center", "right"],
        SingleColumn "bottom"
      ],
    RowLayout
      "grid"
      [ MultiColumn ["top-left", "top", "top-right"],
        MultiColumn ["left", "center", "right"],
        MultiColumn ["bottom-left", "bottom", "bottom-right"]
      ]
  ]

rowAreas :: Row -> [Text]
rowAreas (SingleColumn area) = [area]
rowAreas (MultiColumn areas) = areas

layoutAreas :: Layout -> [Text]
layoutAreas l = concatMap rowAreas $ rows l

hasRowLayout :: Block -> Maybe Layout
hasRowLayout block = do
  let long = attribValue "layout" block >>= findLayout
  let short = map findLayout (classes block)
  listToMaybe $ catMaybes $ long : short
  where
    findLayout l = find ((l =~) . name) layouts

renderRow :: Text -> AreaMap -> Row -> Maybe Block
renderRow lname areaMap (SingleColumn area) =
  lookup area areaMap
    >>= Just . Div ("", ["layout", "row", lname], []) . (: []) . renderColumn . ("",1,)
renderRow lname areaMap (MultiColumn areas) =
  Just $
    Div ("", ["layout", "row", lname], []) $
      mapMaybe renderArea (zip [1 ..] areas)
  where
    renderArea (i, area) = lookup area areaMap >>= Just . renderColumn . (area,i,)

renderColumn :: (Text, Int, [Block]) -> Block
renderColumn (name, i, blocks) =
  Div ("", ["area", name], blocks ^. attributes . attrs) blocks

renderLayout :: AreaMap -> Layout -> [Block]
renderLayout areaMap l = mapMaybe (renderRow (name l) areaMap) (rows l)

slideAreas :: [Text] -> [Block] -> AreaMap
slideAreas names blocks =
  mapMaybe (\area -> firstClass names (Data.List.head area) >>= Just . (,area)) $
    filter (not . null) $
      split (keepDelimsL $ whenElt (hasAnyClass names)) blocks

layoutSlide2 :: Slide -> Decker Slide
layoutSlide2 slide@(Slide (Just header) body dir) = do
  disp <- gets disposition
  case disp of
    Disposition _ Html ->
      case hasRowLayout header of
        Just layout ->
          let names = layoutAreas layout
              areas = slideAreas names body
           in return $ Slide (Just header) (renderLayout areas layout) dir
        Nothing ->
          return $
            Slide
              (Just header)
              [ Div
                  ("", ["layout"], [])
                  [ Div
                      ("", ["area"], [])
                      body
                  ]
              ]
              dir
    Disposition _ _ -> return slide
layoutSlide2 slide = return slide
