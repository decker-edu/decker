module Text.Decker.Filter.Layout
  ( layoutSlide
  ) where

import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Pandoc.Definition ()

import Control.Lens
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Pandoc.Lens
import Text.Read hiding (lift)

-- | Slide layouts are rows of one ore more columns.
data RowLayout = RowLayout
  { lname :: String
  , rows :: [Row]
  } deriving (Eq, Show)

-- | A row consists of one or more columns. 
data Row
  = SingleColumn String
  | MultiColumn [String]
  deriving (Eq, Show)

type Area = [Block]

type AreaMap = [(String, Area)]

rowLayouts :: [RowLayout]
rowLayouts =
  [ RowLayout
      "columns"
      [ SingleColumn "top"
      , MultiColumn ["left", "center", "right"]
      , SingleColumn "bottom"
      ]
  ]

rowAreas :: Row -> [String]
rowAreas (SingleColumn area) = [area]
rowAreas (MultiColumn areas) = areas

layoutAreas :: RowLayout -> [String]
layoutAreas l = concatMap rowAreas $ rows l

hasRowLayout :: Block -> Maybe RowLayout
hasRowLayout block =
  attribValue "layout" block >>= (\l -> find ((==) l . lname) rowLayouts)

renderRow :: AreaMap -> Row -> Maybe Block
renderRow areaMap (SingleColumn area) =
  lookup area areaMap >>= Just . Div ("", ["single-column-row"], [])
renderRow areaMap (MultiColumn areas) =
  Just $
  Div ("", ["multi-column-row", "multi-column-row-" ++ show (length areas)], []) $
  mapMaybe renderArea (zip [1 ..] areas)
  where
    renderArea (i, area) = lookup area areaMap >>= Just . renderColumn . (i, )

renderColumn :: (Int, [Block]) -> Block
renderColumn (i, blocks) =
  let grow =
        fromMaybe (1 :: Int) $ lookup "grow" (blocks ^. attributes . attrs) >>=
        readMaybe
   in Div
        ( ""
        , ["grow-" ++ show grow, "column", "column-" ++ show i]
        , blocks ^. attributes . attrs)
        blocks

renderLayout :: AreaMap -> RowLayout -> [Block]
renderLayout areaMap l = mapMaybe (renderRow areaMap) (rows l)

slideAreas :: [String] -> [Block] -> AreaMap
slideAreas names blocks =
  mapMaybe (\area -> firstClass names (head area) >>= Just . (, area)) $
  filter (not . null) $
  split (keepDelimsL $ whenElt (hasAnyClass names)) blocks

layoutSlide :: Slide -> Decker Slide
layoutSlide slide@(Slide (Just header) body) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html ->
      case hasRowLayout header of
        Just layout ->
          let names = layoutAreas layout
              areas = slideAreas names body
           in return $ Slide (Just header) $ renderLayout areas layout
        Nothing -> return slide
    Disposition Handout Html ->
      case hasRowLayout header of
        Just layout ->
          let names = layoutAreas layout
              areas = slideAreas names body
           in return $ Slide (Just header) $ renderLayout areas layout
        Nothing -> return slide
    Disposition _ _ -> return slide
layoutSlide slide = return slide
