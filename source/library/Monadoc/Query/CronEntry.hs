{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.CronEntry where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Timestamp as Timestamp

selectAll :: MonadSql.MonadSql m => m [CronEntry.Model]
selectAll = MonadSql.query_ "select * from cronEntry"

selectByGuid :: MonadSql.MonadSql m => Guid.Guid -> m (Maybe CronEntry.Model)
selectByGuid guid = do
  cronEntries <- MonadSql.query "select * from cronEntry where guid = ? limit 1" [guid]
  pure $ Maybe.listToMaybe cronEntries

selectByKey :: MonadSql.MonadSql m => CronEntry.Key -> m (Maybe CronEntry.Model)
selectByKey key = do
  cronEntries <- MonadSql.query "select * from cronEntry where key = ? limit 1" [key]
  pure $ Maybe.listToMaybe cronEntries

selectNext :: MonadSql.MonadSql m => Timestamp.Timestamp -> m (Maybe CronEntry.Model)
selectNext now = do
  rows <- MonadSql.query "select * from cronEntry where runAt <= ? order by runAt asc limit 1" [now]
  pure $ Maybe.listToMaybe rows

selectWithGuid :: MonadSql.MonadSql m => m [CronEntry.Model]
selectWithGuid = MonadSql.query_ "select * from cronEntry where guid is not null"
