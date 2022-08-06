module Monadoc.Action.Package.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Package.Package -> App.App Package.Model
run package = do
  r1 <-
    App.Sql.query
      "select key from package where name = ? limit 1"
      [Package.name package]
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into package (name) values (?) returning key" package
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = package}
