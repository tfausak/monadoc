{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.CronEntry where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Guid as Guid

selectByGuid :: MonadSql.MonadSql m => Guid.Guid -> m (Maybe CronEntry.Model)
selectByGuid guid = do
  cronEntries <- MonadSql.query "select * from cronEntry where guid = ? limit 1" [guid]
  pure $ Maybe.listToMaybe cronEntries
