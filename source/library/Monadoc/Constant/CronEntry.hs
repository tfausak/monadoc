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
      }
  ]