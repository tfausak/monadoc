module Monadoc.Action.Preference.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Preference.Preference -> App.App Preference.Model
run preference = do
  rows <- App.Sql.query "select key from preference where package = ?" [Preference.package preference]
  key <- case rows of
    [] -> do
      App.Sql.execute "insert into preference (package, range) values (?, ?)" preference
      Key.SelectLastInsert.run
    Sql.Only key : _ -> do
      App.Sql.execute "update preference set range = ? where key = ?" (Preference.range preference, key)
      pure key
  pure Model.Model {Model.key = key, Model.value = preference}
