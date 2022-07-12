{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Module where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.ModuleName as ModuleName

selectByName :: ModuleName.ModuleName -> App.App (Maybe Module.Model)
selectByName name = do
  modules <- App.query "select * from module where name = ? limit 1" [name]
  pure $ Maybe.listToMaybe modules
