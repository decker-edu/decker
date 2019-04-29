{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
{-# LANGUAGE CPP #-}

module Text.Decker.Flags
  ( hasPreextractedResources
  ) where

hasPreextractedResources :: Bool
#ifdef PREEXTRACTEDRESOURCES
hasPreextractedResources = True
#else
hasPreextractedResources = False
#endif
