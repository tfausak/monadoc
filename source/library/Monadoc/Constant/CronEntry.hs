module Monadoc.Constant.CronEntry where

import qualified Data.UUID as Uuid
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Type.Task as Task
import qualified System.Cron as Cron
import qualified Witch

all :: [CronEntry.CronEntry]
all =
  [ CronEntry.CronEntry
      { CronEntry.guid = Just . Witch.from $ Uuid.fromWords64 0x6b110bd181cb4ce2 0xa331694cd088be96,
        CronEntry.runAt = Witch.from Time.epoch,
        CronEntry.schedule = Witch.from Cron.daily,
        CronEntry.task = Task.Vacuum
      },
    CronEntry.CronEntry
      { CronEntry.guid = Just . Witch.from $ Uuid.fromWords64 0xae002a3a3c4f47b7 0xbd4109231f655721,
        CronEntry.runAt = Witch.from Time.epoch,
        CronEntry.schedule = Witch.from Cron.everyMinute,
        CronEntry.task = Task.PruneHackageIndex
      },
    CronEntry.CronEntry
      { CronEntry.guid = Just . Witch.from $ Uuid.fromWords64 0x0baa03dc275b4cfe 0x8675199ac671132f,
        CronEntry.runAt = Witch.from Time.epoch,
        CronEntry.schedule = Witch.from Cron.everyMinute,
        CronEntry.task = Task.ProcessHackageIndex
      },
    CronEntry.CronEntry
      { CronEntry.guid = Just . Witch.from $ Uuid.fromWords64 0xde9e88dfd68549cb 0xb2c92386d3d388bc,
        CronEntry.runAt = Witch.from Time.epoch,
        CronEntry.schedule = Witch.from Cron.everyMinute,
        CronEntry.task = Task.ProcessUploads
      }
  ]

{-
    CronEntry.CronEntry
      { CronEntry.guid = Just . Witch.from $ Uuid.fromWords64 0xf6b75bb337014b13 0xa30acff83b35150b,
        CronEntry.runAt = Witch.from Time.epoch,
        CronEntry.schedule = Witch.from Cron.everyMinute,
        CronEntry.task = Task.UpsertHackageIndex
      },
-}
