module Monadoc.Action.Module.Upsert where

import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Query.Module as Module
import qualified Monadoc.Type.App as App

run :: Module.Module -> App.App Module.Model
run module_ = do
  maybeModel <- Module.selectByName $ Module.name module_
  case maybeModel of
    Just model -> pure model
    Nothing -> Module.Insert.run module_
