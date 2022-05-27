{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Delete where

import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Type.Guid as Guid

run :: MonadSql.MonadSql m => Guid.Guid -> m ()
run guid = MonadSql.execute "delete from cronEntry where guid = ?" [guid]
