#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
-- dot.hs

module Main where

import qualified Data.ByteString.Base64          as B64
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Digest.Pure.MD5
import           Data.List
import           Data.List.Split
import           System.Exit
import           System.IO
import           System.Process
import           Text.Pandoc.JSON

-- All supported Graphviz render
graphviz :: [String]
graphviz = ["dot", "neato", "twopi", "circo", "fdp", "sfdp", "patchwork"]

-- Searches code block attributes for the first graphviz renderer command
parseAttribs :: Attr -> Maybe (String, String)

parseAttribs (_, _, namevals) = find (((flip elem) graphviz) . fst) namevals

-- Compiles an external Graphviz file to an external PDF file. Returns the name of
-- the PDF file or an error message.
compileExternal :: String -> String -> IO (Either String String)

compileExternal cmd infile = do
  let outfile = (Prelude.head $ splitOn "." infile) ++ ".pdf"
  result <- readProcessWithExitCode cmd ["-Tpdf", "-o", outfile, infile] ""
  case result of
    (ExitSuccess, _, _) -> return (Right outfile)
    (_, err, _) -> return (Left err)

-- Compiles an external Graphviz file to an external PDF file with a generated filename.
-- Returns the name of the PDF file or an error message.
compileInternal :: String -> String -> IO (Either String String)

compileInternal cmd contents = do
  let outfile = cmd ++ "-" ++  (take 8 $ show $ md5 $ L.pack contents) ++ ".pdf"
  result <- readProcessWithExitCode cmd ["-Tpdf", "-o", outfile] contents
  case result of
    (ExitSuccess, _, _) -> return (Right outfile)
    (_, err, _) -> return (Left err)

-- Creates a Pandoc Image block from the filename or communicates an inline error message.
generateBlock :: (Either String String) -> IO Block

generateBlock (Right filename) = do
  return (Para [Image nullAttr [] (filename, "Generated from code block")])
generateBlock (Left error) = do
  hPutStrLn stderr msg
  return (Para [Str msg])
         where msg = "Error in filter 'graphviz': " ++ error

-- Compiles graphviz code from a code block to an image block
compileGraphviz :: Maybe Format -> Block -> IO Block

compileGraphviz (Just (Format "latex")) cb@(CodeBlock attribs contents) =
    case parseAttribs attribs of
      Just (graphvizCmd, "")     -> compileInternal graphvizCmd contents >>= generateBlock
      Just (graphvizCmd, infile) -> compileExternal graphvizCmd infile >>= generateBlock
      Nothing                    -> return cb

compileGraphviz (Just (Format "revealjs")) cb@(CodeBlock (id, classes, namevals) contents) =
    -- Examine 'dot' attribute.
    case lookup "dot" namevals of
      -- Empty file name means 'read from code block'.
      Just "" -> do
        -- Pipe content to dot, include result via data url on an image tag.
        -- Otherwise it is difficult to control the size of the resulting SVG
        -- element with CSS.
        (exitCode, svg, err) <- readProcessWithExitCode "dot" ["-Tsvg"] contents
        if exitCode == ExitSuccess
        then return (Para [Image nullAttr [] (svgDataUrl svg, "Generated from code block")])
        else return (Para [Str $ "Error running 'dot': " ++ err])

      -- Read from file
      Just file -> do
        (exitCode, svg, err) <- readProcessWithExitCode "dot" ["-Tsvg", file] ""
        if exitCode == ExitSuccess
        then return (Para [Image nullAttr [] (svgDataUrl svg, "Generated from file " ++ file)])
        else return (Para [Str $ "Error running 'dot': " ++ err])

      -- Do nothing
      Nothing -> return cb

compileGraphviz _ cb = return cb

main :: IO ()
main = toJSONFilter compileGraphviz

-- | Encode a svg snippet into a data url for an image element
svgDataUrl :: String -> String
svgDataUrl svg = "data:image/svg+xml;base64," ++ B.unpack (B64.encode (B.pack svg))
