module Monadoc.Action.Task.Perform where

import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Prune as HackageIndex.Prune
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Action.Upload.Process as Upload.Process
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Task as Task

run :: Task.Task -> App.App ()
run task = case task of
  Task.ProcessHackageIndex -> HackageIndex.Process.run
  Task.ProcessUploads -> Upload.Process.run
  Task.PruneHackageIndex -> HackageIndex.Prune.run
  Task.UpsertHackageIndex -> HackageIndex.Upsert.run
  Task.Vacuum -> Database.Vacuum.run
