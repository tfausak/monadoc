module Monadoc.Action.Migration.Insert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run ::
  Migration.Migration ->
  App.App Migration.Model
run migration = do
  rows <- App.Sql.query "insert into migration (createdAt, query) values (?, ?) returning key" migration
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = migration}
