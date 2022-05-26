{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.CronEntry.SelectByGuid where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Guid as Guid

run :: MonadSql.MonadSql m => Guid.Guid -> m (Maybe CronEntry.Model)
run guid = do
  cronEntries <- MonadSql.query "select * from cronEntry where guid = ? limit 1" [guid]
  pure $ Maybe.listToMaybe cronEntries
