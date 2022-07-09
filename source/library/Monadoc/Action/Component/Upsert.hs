module Monadoc.Action.Component.Upsert where

import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Query.Component as Component
import qualified Monadoc.Type.App as App

run :: Component.Component -> App.App Component.Model
run component = do
  maybeModel <- Component.selectByTypeAndName (Component.type_ component) (Component.name component)
  case maybeModel of
    Just model -> pure model
    Nothing -> Component.Insert.run component
