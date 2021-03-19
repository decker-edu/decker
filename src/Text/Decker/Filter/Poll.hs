{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Filter.Poll (handlePolls) where
import Data.Aeson 
import Data.Aeson.TH
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Yaml as Y
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Local
import Text.Decker.Internal.Common
import Text.Decker.Filter.Slide 
import Text.Decker.Internal.Meta as M 
import Text.Pandoc.Definition

data PollMeta = PollMeta
    { label :: String
    , color :: String
    , timed :: Bool
    , seconds :: String
    , blink :: Bool
    } deriving Show

-- Look in YAML for poll:true to see if deck has poll
handlePolls :: Pandoc -> Decker Pandoc
handlePolls pandoc@(Pandoc meta blocks) = 
    case (M.lookupMeta "poll" meta :: Maybe MetaValue) of
        Just poll -> return $ Pandoc meta (fromSlides $ parseSlides $ toSlides blocks)
        _ -> return pandoc

-- Look through deck for poll response polls (.poll in H1)
parseSlides :: [Slide] -> [Slide]
parseSlides (sl@(Slide (Just (Header _ (_, tgs, _) _))_):slides) = 
    if "poll" `elem` tgs 
        then buildPoll sl ++ parseSlides slides
    else sl : parseSlides slides
parseSlides (s:sl) = parseSlides sl
parseSlides [] = []

-- Build and attach results slide
buildPoll :: Slide -> [Slide]
buildPoll (Slide (Just (Header l (i, t, k) title)) body) =
    Slide (Just (Header l (i, t, k) (icon:title))) (timer:body):[results]
    where
        pm = buildPollMeta $ getYaml body
        ti = if timed pm then T.pack "timed" else ""
        bl = if blink pm then T.pack "blink" else ""
        timer = Div ("", ["countdown", ti, bl], [("data-seconds", T.pack $ seconds pm)]) []
        icon = RawInline (Format "html") "<i class=\"fas fa-qrcode\"></i>"
        results = Slide (Just resultsHead) [chart]
        resultsHead = Header 1 ("", ["sub"], []) [Str $ T.pack "Poll Results"]
        chart = renderCanvas (findQuestions body) pm
buildPoll s = [s]

buildPollMeta :: Maybe Meta -> PollMeta
buildPollMeta meta = case meta of 
    Just m -> PollMeta lab col tim sec bl
        where
            lab = lookupMetaOrElse "Number of Votes" "label" m
            col = lookupMetaOrElse "#008cff" "color" m
            tim = lookupMetaOrElse False "timed" m
            sec = lookupMetaOrElse "11" "seconds" m 
            bl = lookupMetaOrElse False "blink" m
    _ -> PollMeta "Number of Votes" "#008cff" False "60" False

-- Define default pollMeta if some or no yaml values are found
getYaml :: [Block] -> Maybe Meta
getYaml ((Div a b):bls) = buildMeta b 
getYaml (b:bls) = getYaml bls
getYaml [] = Nothing

-- parse the yaml block in the slide for meta data
buildMeta :: [Block] -> Maybe Meta
buildMeta ((CodeBlock (_,tgs,_) code):bl) = 
    if "yaml" `elem` tgs then 
        case decodeEither' (encodeUtf8 code) of
            Right a -> Just $ toPandocMeta a
            Left exception -> Nothing
    else buildMeta bl
buildMeta (b:bl) = buildMeta bl
buildMeta [] = Nothing

-- Parse slide body to find question block
findQuestions :: [Block] -> [String]
findQuestions (poll@(Div (i, tgs, k) qns):body) = 
    if any (`elem` tgs) ["qmc", "quiz-mc", "quiz-multiple-choice"] 
        then parseAnswers qns
        else findQuestions body
findQuestions (b:body) = findQuestions body 
findQuestions [] = ["Error building answers"]

-- Parse question block to get answers
parseAnswers :: [Block] -> [String]
parseAnswers ((BulletList list):bl) = map buildAnswers list 
parseAnswers (b:bl) = parseAnswers bl
parseAnswers [] = ["No answers found"]

-- Build list of answers
buildAnswers :: [Block] -> String
buildAnswers block = 
    case block of 
        -- (Plain (a : Space : [Str ans]) : b) -> T.unpack ans
        (Plain [Str ans] : a) -> T.unpack ans
        (Plain (a: Space : ans) : b) -> concatMap mapAnswer ans
        a -> "No answer found"
        where
            mapAnswer ans = 
                case ans of 
                    Str a -> T.unpack a
                    Space -> " "
                    a -> ""

data ChartObj = ChartObj
    { chartdata :: DataObj
    , chartoptions :: OptionsObj
    }
data DataObj = DataObj
    { labels :: [String]
    , datasets :: [DatasetObj]
    }
data DatasetObj = DatasetObj
    { dslabel :: String
    , dsbackgroundColor :: String
    , dsdata :: [Integer]
    } 
newtype OptionsObj = OptionsObj 
    {optresponsive :: String}  

-- Build canvas tag with chart comment to render results of poll
renderCanvas :: [String] -> PollMeta -> Block 
renderCanvas answers pm = 
    rawHtml' $ 
        H.canvas ! A.class_ "stretch" ! 
        H.dataAttribute "chart" "bar" $ 
        unsafeByteStringComment $ 
        toStrict $ Data.Aeson.encode chartObj
    where
        datasetObj = DatasetObj (label pm) (color pm) (map (const 0) answers)
        chartObj = ChartObj (DataObj answers [datasetObj]) (OptionsObj "true")

deriveJSON defaultOptions ''PollMeta
deriveJSON defaultOptions { fieldLabelModifier = drop 5 } ''ChartObj
deriveJSON defaultOptions ''DataObj
deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''DatasetObj
deriveJSON defaultOptions { fieldLabelModifier = drop 3 } ''OptionsObj