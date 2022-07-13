{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.CronEntry where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Timestamp as Timestamp

selectAll :: App.App [CronEntry.Model]
selectAll = App.Sql.query_ "select * from cronEntry"

selectByGuid :: Guid.Guid -> App.App (Maybe CronEntry.Model)
selectByGuid guid = do
  cronEntries <- App.Sql.query "select * from cronEntry where guid = ? limit 1" [guid]
  pure $ Maybe.listToMaybe cronEntries

selectByKey :: CronEntry.Key -> App.App (Maybe CronEntry.Model)
selectByKey key = do
  cronEntries <- App.Sql.query "select * from cronEntry where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe cronEntries

selectNext :: Timestamp.Timestamp -> App.App (Maybe CronEntry.Model)
selectNext now = do
  rows <- App.Sql.query "select * from cronEntry where runAt <= ? order by runAt asc limit 1" [now]
  pure $ Maybe.listToMaybe rows

selectWithGuid :: App.App [CronEntry.Model]
selectWithGuid = App.Sql.query_ "select * from cronEntry where guid is not null"
