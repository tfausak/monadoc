module Monadoc.Action.License.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.License as License
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: License.License -> App.App License.Model
run license = do
  r1 <-
    App.Sql.query
      "select key from license where spdx = ? limit 1"
      [license.spdx]
  key <- case r1 of
    Sql.Only key : _ -> pure key
    [] -> do
      r2 <- App.Sql.query "insert into license (spdx) values (?) returning key" license
      case r2 of
        Sql.Only key : _ -> pure key
        [] -> Traced.throw MissingKey.MissingKey
  pure Model.Model {Model.key = key, Model.value = license}
