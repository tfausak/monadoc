module Monadoc.Action.CronEntry.Delete where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Guid as Guid

run :: Guid.Guid -> App.App ()
run guid = App.Sql.execute "delete from cronEntry where guid = ?" [guid]
