module Monadoc.Query.CronEntry where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Guid as Guid

getByGuid :: Guid.Guid -> App.App (Maybe CronEntry.Model)
getByGuid =
  fmap Maybe.listToMaybe
    . App.Sql.query "select * from cronEntry where guid = ? limit 1"
    . List.singleton
