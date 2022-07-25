module Monadoc.Action.Key.SelectLastInsert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingRowid as MissingRowid
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key

run :: App.App (Key.Key a)
run = do
  rows <- App.Sql.query_ "select last_insert_rowid()"
  case rows of
    Sql.Only key : _ | key /= Key.zero -> pure key
    _ -> Traced.throw MissingRowid.MissingRowid
