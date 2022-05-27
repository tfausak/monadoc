{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Update where

import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Model as Model

run :: MonadSql.MonadSql m => CronEntry.Model -> m ()
run cronEntry =
  MonadSql.execute
    "update cronEntry set guid = ?, runAt = ?, schedule = ?, task = ? where key = ?"
    ( CronEntry.guid $ Model.value cronEntry,
      CronEntry.runAt $ Model.value cronEntry,
      CronEntry.schedule $ Model.value cronEntry,
      CronEntry.task $ Model.value cronEntry,
      Model.key cronEntry
    )
