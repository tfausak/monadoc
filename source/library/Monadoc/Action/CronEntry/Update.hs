{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Update where

import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: CronEntry.Model -> App.App ()
run cronEntry =
  App.Sql.execute
    "update cronEntry set guid = ?, runAt = ?, schedule = ?, task = ? where key = ?"
    ( CronEntry.guid $ Model.value cronEntry,
      CronEntry.runAt $ Model.value cronEntry,
      CronEntry.schedule $ Model.value cronEntry,
      CronEntry.task $ Model.value cronEntry,
      Model.key cronEntry
    )
