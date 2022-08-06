module Monadoc.Action.Preference.Upsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Preference.Preference -> App.App Preference.Model
run preference = do
  rows <-
    App.Sql.query
      "insert into preference ( package, range ) values ( ?, ? ) \
      \ on conflict ( package ) do update set range = excluded.range \
      \ returning key"
      preference
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = preference}
