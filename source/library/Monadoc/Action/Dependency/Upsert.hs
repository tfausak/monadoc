module Monadoc.Action.Dependency.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Dependency.Dependency -> App.App Dependency.Model
run dependency = do
  rows <-
    App.Sql.query
      "insert into dependency ( packageMetaComponent, package, component, range ) values ( ?, ?, ?, ? ) \
      \ on conflict ( packageMetaComponent, package, component ) do update set range = excluded.range \
      \ returning key"
      dependency
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = dependency}
