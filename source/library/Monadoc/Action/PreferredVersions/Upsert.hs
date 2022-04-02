module Monadoc.Action.PreferredVersions.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.PreferredVersions as PreferredVersions
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => PreferredVersions.PreferredVersions -> m PreferredVersions.Model
run preferredVersions = do
  rows <- MonadSql.query "select key from preferredVersions where package = ?" [PreferredVersions.package preferredVersions]
  key <- case rows of
    [] -> do
      MonadSql.execute "insert into preferredVersions (package, range) values (?, ?)" preferredVersions
      Key.SelectLastInsert.run
    Sql.Only key : _ -> do
      MonadSql.execute "update preferredVersions set range = ? where key = ?" (PreferredVersions.range preferredVersions, key)
      pure key
  pure Model.Model {Model.key = key, Model.value = preferredVersions}
