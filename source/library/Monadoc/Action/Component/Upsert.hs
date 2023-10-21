module Monadoc.Action.Component.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Component.Component -> App.App Component.Model
run component = do
  r1 <-
    App.Sql.query
      "select key from component where type = ? and name = ? limit 1"
      (component.type_, component.name)
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into component (type, name) values (?, ?) returning key" component
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = component}
