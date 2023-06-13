module Monadoc.Action.Package.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Package.Package -> App.App Package.Model
run package = do
  maybeModel <-
    Package.Query.getByName $ Package.name package
  case maybeModel of
    Just model -> pure model
    Nothing -> do
      rows <- App.Sql.query "insert into package (name) values (?) returning key" package
      case rows of
        Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = package}
        [] -> Traced.throw MissingKey.MissingKey
