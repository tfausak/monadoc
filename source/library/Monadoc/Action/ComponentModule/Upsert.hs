module Monadoc.Action.ComponentModule.Upsert where

import qualified Monadoc.Action.ComponentModule.Insert as ComponentModule.Insert
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Query.ComponentModule as ComponentModule
import qualified Monadoc.Type.App as App

run ::
  ComponentModule.ComponentModule ->
  App.App ComponentModule.Model
run component = do
  maybeModel <-
    ComponentModule.selectByComponentAndModule
      (ComponentModule.component component)
      (ComponentModule.module_ component)
  case maybeModel of
    Just model -> pure model
    Nothing -> ComponentModule.Insert.run component
