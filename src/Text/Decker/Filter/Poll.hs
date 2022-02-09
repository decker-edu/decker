{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Filter.Poll (handlePolls) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Lazy.Internal as L
import qualified Data.Text as T
import Data.Text.Encoding as E 
import Data.Yaml as Y (decodeEither')
import Text.Decker.Filter.Slide (tag)
import Text.Decker.Internal.Common (Decker)
import Text.Decker.Internal.Meta as M
    ( toPandocMeta, lookupMetaOrElse, lookupMeta )
import Text.Pandoc.Definition
import Text.Pandoc.Walk


data PollMeta = PollMeta
  { color :: String,
    timed :: Bool,
    seconds :: String,
    options :: OptionsObj
  } deriving (Show)

data Chart = Chart
  { chartdata :: DataObj,
    chartoptions :: OptionsObj
  } deriving (Show)

data DataObj = DataObj
  { labels :: [String],
    datasets :: [Dataset]
  } deriving (Show)

data Dataset = Dataset
  { dsbackgroundColor :: String,
    dsdata :: [Integer]
  } deriving (Show)

data OptionsObj = OptionsObj
  { optscales :: Scales,
    optplugins :: Plugin
  } deriving (Show)

data Scales = Scales
  { yAxes :: Axes,
    xAxes :: Axes
  } deriving (Show)

data Ticks = Ticks 
  { tickbeginAtZero :: Bool,
    tickstepSize :: Int, 
    tickfontColor :: String,
    tickfontSize :: Int,
    tickfontStyle :: String
  } deriving (Show)

newtype Axes = Axes { ticks :: Ticks } deriving (Show)
newtype Plugin = Plugin { pluglegend :: Legend } deriving Show
newtype Legend = Legend Disp deriving (Show)
newtype Disp = Disp { display :: Bool } deriving (Show)

-- Look in YAML for poll:true to see if deck has poll, 
-- then look in slides for 'poll' to parse poll slides and build results chart
handlePolls :: Pandoc -> Decker Pandoc
handlePolls pandoc@(Pandoc meta blocks) =
  case (M.lookupMeta "poll" meta :: Maybe MetaValue) of
    Just poll -> return $ walk parseBlocks pandoc
    _ -> return pandoc
  where
    parseBlocks :: Block -> Block
    parseBlocks (Div a@(id_, cls, kvs) blocks) 
      | "poll" `elem` cls = Div a (timer : parsed ++ [chart])
        where
          parsed = walk parsePolls blocks
          pm = buildPollMeta $ getYaml blocks
          ti = if timed pm then T.pack "timed" else ""
          timer = Div ("", ["countdown", ti], [("data-seconds", T.pack $ seconds pm)]) []
          chart = renderCanvas (findQuestions $ head blocks) pm
    parseBlocks bl = bl
    parsePolls :: Block -> Block
    parsePolls (Header 1 a title) = 
      Header 1 a (RawInline (Format "html") "<i class=\"fas fa-qrcode\"></i>" : title)
    parsePolls b = b

-- recursively search for questions to build answers for results chart
findQuestions :: Block -> [String]
findQuestions (Div (_,tgs,_) divs) 
  | any (`elem` tgs) ["qmc", "quiz-mc", "quiz-multiple-choice"] =
    concatMap parseQuestions divs
  | otherwise = concatMap findQuestions divs
  where
    parseQuestions :: Block -> [String]
    parseQuestions (BulletList ans) = map parseAnswers ans
    parseQuestions b = []
    -- each answer block begins with icon and space 
    parseAnswers :: [Block] -> String
    parseAnswers (Plain (_:_:ils) : bl) = 
      concatMap cleanAnswer ils ++ parseAnswers bl
    parseAnswers b = []
    cleanAnswer ans =
      case ans of
        Str a -> T.unpack a
        Space -> " "
        a -> []
findQuestions d = [] 


buildPollMeta :: Maybe Meta -> PollMeta
buildPollMeta meta = case meta of
  Just m -> PollMeta col tim sec optObj
    where
      col = lookupMetaOrElse "#008cff" "color" m
      tim = lookupMetaOrElse False "timed" m
      sec = lookupMetaOrElse "11" "seconds" m
      color = lookupMetaOrElse "#000" "font-color" m
      size = lookupMetaOrElse 18 "font-size" m
      style = lookupMetaOrElse "bold" "font-style" m
      ticks = Ticks True 1 color size style
      sc = Scales (Axes ticks) (Axes ticks)
      optObj = OptionsObj sc $ Plugin $ Legend $ Disp False
  _ -> PollMeta "#008cff" False "60" optObj
    where
      ticks = Ticks True 1 "#000" 18 "bold"
      sc = Scales (Axes ticks) (Axes ticks)
      optObj = OptionsObj sc $ Plugin $ Legend $ Disp False

-- define default pollMeta if some or no yaml values are found
getYaml :: [Block] -> Maybe Meta
getYaml ((Div a b) : bls) = buildMeta b
getYaml (b : bls) = getYaml bls
getYaml [] = Nothing

-- parse the yaml block in the slide for meta data
buildMeta :: [Block] -> Maybe Meta
buildMeta ((CodeBlock (_, tgs, _) code) : bl) =
  if "yaml" `elem` tgs
    then case decodeEither' (encodeUtf8 code) of
      Right a -> Just $ toPandocMeta a
      Left exception -> Nothing
    else buildMeta bl
buildMeta (b : bl) = buildMeta bl
buildMeta [] = Nothing

-- build canvas tag with chart comment to render results of polls
renderCanvas :: [String] -> PollMeta -> Block
renderCanvas answers pm = Div ("", ["poll_results"], []) [Plain [canvas]]
  where
    canvas = tag "canvas" $ Span ("", ["stretch"], [("data-chart", "bar")]) [open,ch,end]
    open = RawInline "html" $ T.pack "<!-- "
    ch = RawInline "html" $ T.pack chart
    end = RawInline "html" $ T.pack " -->"
    chart = unpackChars $ Data.Aeson.encode chartObj
    dataset = Dataset (color pm) (map (const 0) answers)
    dataO = DataObj answers [dataset]
    chartObj = Chart dataO (options pm)

deriveJSON defaultOptions ''PollMeta
deriveJSON defaultOptions {fieldLabelModifier = drop 5} ''Chart
deriveJSON defaultOptions ''DataObj
deriveJSON defaultOptions {fieldLabelModifier = drop 2} ''Dataset
deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''OptionsObj
deriveJSON defaultOptions ''Scales
deriveJSON defaultOptions {fieldLabelModifier = drop 3} ''Legend
deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Plugin
deriveJSON defaultOptions ''Axes
deriveJSON defaultOptions ''Disp
deriveJSON defaultOptions {fieldLabelModifier = drop 4} ''Ticks
