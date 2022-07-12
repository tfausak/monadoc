{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Module.Insert where

import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run ::
  Module.Module ->
  App.App Module.Model
run module_ = do
  App.execute "insert into module (name) values (?)" module_
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = module_}
