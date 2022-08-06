module Monadoc.Action.CronEntry.Insert where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.MissingKey as MissingKey
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: CronEntry.CronEntry -> App.App CronEntry.Model
run cronEntry = do
  rows <- App.Sql.query "insert into cronEntry (guid, runAt, schedule, task) values (?, ?, ?, ?) returning key" cronEntry
  case rows of
    [] -> Traced.throw MissingKey.MissingKey
    Sql.Only key : _ -> pure Model.Model {Model.key = key, Model.value = cronEntry}
