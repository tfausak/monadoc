module Monadoc.Action.Migration.Migrate where

import qualified Control.Monad as Monad
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Migration.Insert as Migration.Insert
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run ::
  Migration.Migration ->
  App.App Migration.Model
run migration = do
  let createdAt = migration.createdAt
      query = migration.query
  models <- App.Sql.query "select * from migration where createdAt = ? limit 1" [createdAt]
  case models of
    [] -> do
      App.Log.info $ F.sformat ("running migration:" F.%+ F.stext) (Witch.from createdAt)
      App.Sql.execute_ query
      Migration.Insert.run migration
    model : _ -> do
      let oldQuery = model.value.query
      Monad.when (oldQuery /= query) $
        Traced.throw
          Mismatch.Mismatch
            { Mismatch.expected = oldQuery,
              Mismatch.actual = query
            }
      pure model
