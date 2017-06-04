#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- gnuplot.hs
module Main where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import Data.List
import Data.List.Split
import System.Directory
import System.Exit
import System.IO
import System.Process
import Text.Pandoc.JSON

-- Compiles an external Gnuplot file to an external PDF file. Returns the name of
-- the PDF file or an error message.
compileExternal :: String -> String -> IO (Either String String)
compileExternal cmd infile = do
  let outfile = (Prelude.head (splitOn "." infile)) ++ ".pdf"
  let prelude = "set terminal pdfcairo; set output '" ++ outfile ++ "';"
  (exitCode, _, err) <-
    readProcessWithExitCode "gnuplot" ["-d", "-e", prelude, infile] ""
  if exitCode == ExitSuccess
    then return (Right outfile)
    else return (Left err)

genUniqueFilename :: String -> String -> String -> FilePath
genUniqueFilename prefix contents extension =
  prefix ++ "-" ++ (take 8 $ show $ md5 $ L.pack contents) ++ "." ++ extension

-- Compiles an embedded Gnuplot description to an external PDF file with a generated filename.
-- Returns the name of the PDF file or an error message.
compileInternal :: String -> String -> IO (Either String String)
compileInternal cmd contents = do
  let outfile = genUniqueFilename cmd contents "pdf"
  exists <- doesFileExist outfile
  if exists
    then return (Right outfile)
    else do
      let prelude = "set terminal pdfcairo; set output '" ++ outfile ++ "';"
      (exitCode, _, err) <-
        readProcessWithExitCode "gnuplot" ["-d"] (prelude ++ contents)
      if exitCode == ExitSuccess
        then return (Right outfile)
        else return (Left err)

-- Creates a Pandoc Image block from the filename or communicates an inline error message.
generateBlock :: (Either String String) -> IO Block
generateBlock (Right filename) = do
  return (Para [Image nullAttr [] (filename, "Generated from code block")])
generateBlock (Left error) = do
  hPutStrLn stderr msg
  return (Para [Str msg])
  where
    msg = "Error in filter 'gnuplot': " ++ error

thrd (_, _, x) = x

-- Compiles gnuplot code from a code block to an image block
compileGnuplot :: Maybe Format -> Block -> IO Block
compileGnuplot (Just (Format "latex")) cb@(CodeBlock attribs contents) =
  case lookup "gnuplot" (thrd attribs) of
    Just "block" -> compileInternal "gnuplot" contents >>= generateBlock
    Just infile -> compileExternal "gnuplot" infile >>= generateBlock
    Nothing -> return cb
compileGnuplot (Just (Format "revealjs")) cb@(CodeBlock (id, classes, namevals) contents)
    -- Examine 'dot' attribute.
 =
  case lookup "dot" namevals
      -- Empty file name means 'read from code block'.
        of
    Just ""
        -- Pipe content to dot, include result via data
        -- url on an image tag. Otherwise it is difficult to control
        -- the size if the resulting SVG element with CSS.
     -> do
      (exitCode, svg, err) <- readProcessWithExitCode "dot" ["-Tsvg"] contents
      if exitCode == ExitSuccess
        then return
               (Para
                  [ Image
                      nullAttr
                      []
                      (svgDataUrl svg, "Generated from code block")
                  ])
        else return (Para [Str $ "Error running 'dot': " ++ err])
      -- Read from file
    Just file -> do
      (exitCode, svg, err) <- readProcessWithExitCode "dot" ["-Tsvg", file] ""
      if exitCode == ExitSuccess
        then return
               (Para
                  [ Image
                      nullAttr
                      []
                      (svgDataUrl svg, "Generated from file " ++ file)
                  ])
        else return (Para [Str $ "Error running 'dot': " ++ err])
      -- Do nothing
    Nothing -> return cb
compileGnuplot _ cb = return cb

main :: IO ()
main = toJSONFilter compileGnuplot

-- | Encode a svg snippet into a data url for an image element
svgDataUrl :: String -> String
svgDataUrl svg =
  "data:image/svg+xml;base64," ++ (B.unpack (B64.encode (B.pack svg)))
