module Monadoc.Action.Preference.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Preference.Preference -> m Preference.Model
run preference = do
  rows <- MonadSql.query "select key from preference where package = ?" [Preference.package preference]
  key <- case rows of
    [] -> do
      MonadSql.execute "insert into preference (\"constraint\", package) values (?, ?)" preference
      Key.SelectLastInsert.run
    Sql.Only key : _ -> do
      MonadSql.execute "update preference set \"constraint\" = ? where key = ?" (Preference.constraint preference, key)
      pure key
  pure Model.Model {Model.key = key, Model.value = preference}
