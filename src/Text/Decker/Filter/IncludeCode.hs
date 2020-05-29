{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.Decker.Filter.IncludeCode
  ( InclusionMode(..)
  , InclusionSpec(..)
  , parseInclusionUrl
  , includeCode
  ) where

import Text.Decker.Internal.Common

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake
import Network.URI
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Read (readMaybe)

import Text.Decker.Resource.Resource

type LineNumber = Int

data Range = Range
  { rangeStart :: LineNumber
  , rangeEnd :: LineNumber
  } deriving (Show, Eq)

mkRange :: LineNumber -> LineNumber -> Maybe Range
mkRange s e
  | s > 0 && e > 0 && s <= e = Just (Range s e)
  | otherwise = Nothing

data InclusionMode
  = SnippetMode Text
  | SnippetMode1 Text
  | RangeMode Range
  | EntireFileMode
  deriving (Show, Eq)

data InclusionSpec = InclusionSpec
  { include :: FilePath
  , mode :: InclusionMode
  , dedent :: Maybe Int
  } deriving (Show, Eq)

data MissingRangePart
  = Start
  | End
  deriving (Show, Eq)

data InclusionError
  = InvalidRange LineNumber
                 LineNumber
  | IncompleteRange MissingRangePart
  | ConflictingModes [InclusionMode]
  deriving (Show, Eq)

newtype InclusionState = InclusionState
  { startLineNumber :: Maybe LineNumber
  } deriving (Show)

newtype Inclusion a = Inclusion
  { runInclusion :: ReaderT InclusionSpec (StateT InclusionState (ExceptT InclusionError IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader InclusionSpec
             , MonadError InclusionError
             , MonadState InclusionState
             )

runInclusion' ::
     InclusionSpec
  -> Inclusion a
  -> IO (Either InclusionError (a, InclusionState))
runInclusion' spec action =
  runExceptT (runStateT (runReaderT (runInclusion action) spec) initialState)
  where
    initialState = InclusionState {startLineNumber = Nothing}

parseInclusion ::
     HashMap String String -> Either InclusionError (Maybe InclusionSpec)
parseInclusion attrs =
  case HM.lookup "include" attrs of
    Just include -> do
      rangeMode <- parseRangeMode
      mode <-
        case catMaybes [rangeMode, snippetMode] of
          [] -> return EntireFileMode
          [m] -> return m
          ms -> throwError (ConflictingModes ms)
      return (Just InclusionSpec {..})
    Nothing -> return Nothing
  where
    lookupInt name = HM.lookup name attrs >>= readMaybe
    snippetMode = SnippetMode . Text.pack <$> HM.lookup "snippet" attrs
    dedent = lookupInt "dedent"
    parseRangeMode =
      case (lookupInt "startLine", lookupInt "endLine") of
        (Just start, Just end) ->
          maybe
            (throwError (InvalidRange start end))
            (return . Just . RangeMode)
            (mkRange start end)
        (Nothing, Just _) -> throwError (IncompleteRange Start)
        (Just _, Nothing) -> throwError (IncompleteRange End)
        (Nothing, Nothing) -> return Nothing

parseInclusionUrl :: String -> Either InclusionError (Maybe InclusionSpec)
parseInclusionUrl urlString =
  case parseURIReference urlString of
    Just uri
      | uriScheme uri == "code:" -> do
        let include = uriPath uri
        let mode =
              if uriFragment uri == ""
                then EntireFileMode
                else SnippetMode (Text.pack $ filter (/= '#') $ uriFragment uri)
        return (Just $ InclusionSpec include mode Nothing)
    _ -> return Nothing

type Lines = [Text]

setStartLineNumber :: LineNumber -> Inclusion ()
setStartLineNumber n = modify (\s -> s {startLineNumber = Just n})

-- 8< include-shorter
readIncluded :: Inclusion Text
readIncluded = liftIO . Text.readFile =<< asks include

-- >8
-- 8<| include-even-shorter
isSnippetTag :: Text -> Text -> Text -> Bool
isSnippetTag tag name line =
  mconcat [tag, " ", name] `Text.isSuffixOf` Text.strip line

isUnnamedSnippetTag :: Text -> Text -> Bool
isUnnamedSnippetTag tag line = tag `Text.isSuffixOf` Text.strip line

-- start snippet include-start-end
isSnippetStart :: Text -> Text -> Bool
isSnippetStart name line =
  isSnippetTag "start snippet" name line ||
  isSnippetTag "8<|" name line || isSnippetTag "8<" name line

-- end snippet include-start-end
isSnippetEnd :: Text -> Bool -> Text -> Bool
isSnippetEnd name emptyEnd line =
  isSnippetTag "end snippet" name line ||
  isSnippetTag ">8" name line ||
  isUnnamedSnippetTag ">8" line || (emptyEnd && Text.null (Text.strip line))

includeByMode :: Lines -> Inclusion Lines
includeByMode ls =
  asks mode >>= \case
    SnippetMode name -> do
      let (before, start) = break (isSnippetStart name) ls
          -- index +1 for line number, then +1 for snippet comment line, so +2:
          startLine = length before + 2
      let emptyEnd = isSnippetTag "8<|" name (head start)
      setStartLineNumber startLine
      return (takeWhile (not . isSnippetEnd name emptyEnd) (drop 1 start))
    RangeMode range -> do
      setStartLineNumber (rangeStart range)
      return (take (rangeEnd range - startIndex) (drop startIndex ls))
      where startIndex = pred (rangeStart range)
    EntireFileMode -> return ls
    _ -> return ls

dedentLinesMax :: Lines -> Inclusion Lines
dedentLinesMax ls = do
  let max = maxIndent ls
  return (map (dedentLine max) ls)
  where
    largestPrefix a b =
      case Text.commonPrefixes a b of
        Nothing -> ""
        Just (p, _, _) -> p
    maxIndent = foldr largestPrefix "                             "
    dedentLine max line = fromMaybe line $ Text.stripPrefix max line

modifyAttributes ::
     InclusionState -> [String] -> [(String, String)] -> [(String, String)]
modifyAttributes InclusionState {startLineNumber} classes =
  (++) extraAttrs . filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["include", "startLine", "endLine", "snippet", "dedent"]
    extraAttrs =
      case startLineNumber of
        Just n
          | "numberLines" `elem` classes -> [("startFrom", show n)]
        _ -> []

printAndFail :: InclusionError -> IO a
printAndFail = fail . formatError
  where
    formatError =
      \case
        InvalidRange start end ->
          "Invalid range: " ++ show start ++ " to " ++ show end
        IncompleteRange Start -> "Incomplete range: \"startLine\" is missing"
        IncompleteRange End -> "Incomplete range: \"endLine\" is missing"
        ConflictingModes modes -> "Conflicting modes: " ++ show modes

splitLines :: Text -> Inclusion Lines
splitLines = return . Text.lines

joinLines :: Lines -> Inclusion Text
joinLines = return . Text.unlines

allSteps :: Inclusion Text
allSteps =
  readIncluded >>= splitLines >>= includeByMode >>= dedentLinesMax >>= joinLines

stringTup (k, v) = (Text.unpack k, Text.unpack v)

textTup (k, v) = (Text.pack k, Text.pack v)

includeCodeA' :: Block -> Action (Either InclusionError Block)
includeCodeA' cb@(CodeBlock (id', classes, attrs) _) =
  let attrs' = map stringTup attrs
      classes' = map Text.unpack classes
   in case parseInclusion (HM.fromList attrs') of
        Right (Just spec) -> do
          local <- urlToFilePathIfLocal "." (include spec)
          need [local]
          inclusion <- liftIO $ runInclusion' spec {include = local} allSteps
          case inclusion of
            Left err -> return (Left err)
            Right (contents, state) ->
              return
                (Right
                   (CodeBlock
                      ( id'
                      , classes
                      , map textTup $ modifyAttributes state classes' attrs')
                      contents))
        Right Nothing -> return (Right cb)
        Left err -> return (Left err)
includeCodeA' pi@(Para [Image (id', classes, attrs) _ (url, _)]) =
  let attrs' = map stringTup attrs
      classes' = map Text.unpack classes
   in case parseInclusionUrl (Text.unpack url) of
        Right (Just rawSpec) -> do
          local <- urlToFilePathIfLocal "." (include rawSpec)
          need [local]
          let spec = rawSpec {include = local}
          inclusion <- liftIO $ runInclusion' spec allSteps
          case inclusion of
            Left err -> return (Left err)
            Right (contents, state) ->
              return
                (Right
                   (CodeBlock
                      ( id'
                      , classes
                      , map textTup $ modifyAttributes state classes' attrs')
                      contents))
        Right Nothing -> return (Right pi)
        Left err -> return (Left err)
includeCodeA' x = return (Right x)

-- | A Pandoc filter that includes code snippets from
-- external files. Shake Action version.
includeCodeA :: Maybe Format -> Block -> Action Block
includeCodeA _ block = do
  result <- includeCodeA' block
  case result of
    Left err -> liftIO $ printAndFail err
    Right block -> return block

-- start snippet includeCode
includeCode :: Pandoc -> Decker Pandoc
includeCode (Pandoc meta blocks) = do
  included <- lift $ walkM (includeCodeA Nothing) blocks
  return $ Pandoc meta included
-- end snippet includeCode
