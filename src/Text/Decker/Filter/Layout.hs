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
import qualified Data.Text as Text
import Text.Pandoc.Lens
import Text.Read hiding (lift)

-- | Slide layouts are rows of one ore more columns.
data RowLayout = RowLayout
  { name :: Text.Text
  , rows :: [Row]
  } deriving (Eq, Show)

-- | A row consists of one or more columns. 
data Row
  = SingleColumn Text.Text
  | MultiColumn [Text.Text]
  deriving (Eq, Show)

type Area = [Block]

type AreaMap = [(Text.Text, Area)]

rowLayouts :: [RowLayout]
rowLayouts =
  [ RowLayout
      "columns"
      [ SingleColumn "top"
      , MultiColumn ["left", "center", "right"]
      , SingleColumn "bottom"
      ]
  , RowLayout
      "grid"
      [ MultiColumn ["top-left", "top", "top-right"]
      , MultiColumn ["left", "center", "right"]
      , MultiColumn ["bottom-left", "bottom", "bottom-right"]
      ]
  ]

rowAreas :: Row -> [Text.Text]
rowAreas (SingleColumn area) = [area]
rowAreas (MultiColumn areas) = areas

layoutAreas :: RowLayout -> [Text.Text]
layoutAreas l = concatMap rowAreas $ rows l

hasRowLayout :: Block -> Maybe RowLayout
hasRowLayout block = do
  let long = attribValue "layout" block >>= findLayout
  let short = map findLayout (classes block)
  listToMaybe $ catMaybes $ long : short
  where
    findLayout l = find ((==) l . name) rowLayouts

renderRow :: AreaMap -> Row -> Maybe Block
renderRow areaMap (SingleColumn area) =
  lookup area areaMap >>= Just . Div ("", ["single-column-row"], [])
renderRow areaMap (MultiColumn areas) =
  Just $
  Div
    ( ""
    , [ "multi-column-row"
      , "multi-column-row-" <> Text.pack (show (length areas))
      ]
    , []) $
  mapMaybe renderArea (zip [1 ..] areas)
  where
    renderArea (i, area) = lookup area areaMap >>= Just . renderColumn . (i, )

renderColumn :: (Int, [Block]) -> Block
renderColumn (i, blocks) =
  let grow =
        fromMaybe (1 :: Int) $ lookup "grow" (blocks ^. attributes . attrs) >>=
        (readMaybe . Text.unpack)
   in Div
        ( ""
        , [ "grow-" <> Text.pack (show grow)
          , "column"
          , "column-" <> Text.pack (show i)
          ]
        , blocks ^. attributes . attrs)
        blocks

renderLayout :: AreaMap -> RowLayout -> [Block]
renderLayout areaMap l = mapMaybe (renderRow areaMap) (rows l)

slideAreas :: [Text.Text] -> [Block] -> AreaMap
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
