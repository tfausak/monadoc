module Monadoc.Action.Range.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Range.Range -> App.App Range.Model
run range = do
  r1 <-
    App.Sql.query
      "select key from range where \"constraint\" = ? limit 1"
      [range.constraint]
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into range (\"constraint\") values (?) returning key" range
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = range}
