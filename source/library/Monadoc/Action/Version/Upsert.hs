module Monadoc.Action.Version.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Version as Version.Query
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Version.Version -> App.App Version.Model
run version = do
  maybeModel <- Version.Query.getByNumber version.number
  case maybeModel of
    Just model -> pure model
    Nothing -> do
      rows <- App.Sql.query "insert into version (number) values (?) returning key" version
      case rows of
        Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = version}
        [] -> Traced.throw MissingKey.MissingKey
