module Monadoc.Action.Component.Upsert where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.App as App

run :: Component.Component -> App.App Component.Model
run component = do
  models <- App.Sql.query "select * from component where type = ? and name = ?" (Component.type_ component, Component.name component)
  case models of
    model : _ -> pure model
    [] -> Component.Insert.run component
