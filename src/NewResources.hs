{-- Author: Armin Bernstetter <bernstetter@informatik.uni-wuerzburg.de --}
-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module NewResources
  (
  ) where

import Common
import Project
-- | Plan:
-- resources is already in meta data (read from decker-meta.yaml)
-- see metaA in Shake.hs
-- this metadata is currently only read in Decker.hs and then propagated forward
-- This should stay. Here in NewResources only the ResourceType Datatype should be used.
-- Paths come from Project.hs
-- What does "provision" actually mean in this context?
-- 
-- | TODO: From Decker.hs
-- move functionality from "support" command to here
-- | TODO: from Resource.hs
-- getOldResources (maybe to Project bc it returns Paths?)
-- getResourceString (not sure if this fits here. 
--   maybe add an entire "Output.hs" module?)
-- extractResources
-- unzip
-- writeExampleProject
-- writeResourceFiles
-- cp
-- copyDir
-- 
-- | TODO: From Project.hs
-- copyFileIfNewer
-- fileIsNewer
-- copyResource
-- what about
-- linkResource, absRefResource, relRefResource?
-- 
-- 
-- | TODO: From Utilities.hs
-- getSupportDir (move to Project.hs b/c it returns a path) 
-- provisionResources
-- provisionMetaResource
-- provisionTemplateOverrideSupport
-- provisionTemplateOverrideSupportTopLevel (what do those two do?)
-- provisionResource
-- putCurrentDocument
-- urlToFilePathIfLocal (move to Project?)
