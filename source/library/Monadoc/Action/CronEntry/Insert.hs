{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Insert where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model

run :: (MonadSql.MonadSql m, Exception.MonadThrow m) => CronEntry.CronEntry -> m CronEntry.Model
run cronEntry = do
  MonadSql.execute "insert into cronEntry (guid, runAt, schedule, task) values (?, ?, ?, ?)" cronEntry
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = cronEntry}
