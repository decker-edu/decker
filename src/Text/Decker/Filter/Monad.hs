{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Monad where


import Relude
import Text.Pandoc

-- | WriterOptions and the document meta data are available to all
-- filters. 
data FilterState = FilterState
  { options :: WriterOptions
  , meta :: Meta
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

-- | An associative list representing element attributes.
type AttrMap = [(Text, Text)]

-- | The first set contains the transformed attributes that will be extracted
-- and added to the HTML elements. The second set contains the source
-- attributes as parsed from the Markdown attribute markup. Many functions
-- inside the Attrib monad manipulate one or both attribute sets. 
type AttribState = (Attr, Attr)

-- | The Attrib monad.
type Attrib = StateT AttribState Filter

