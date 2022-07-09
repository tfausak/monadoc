module Monadoc.Worker.Main where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.CronEntry.Enqueue as CronEntry.Enqueue
import qualified Monadoc.Action.CronEntry.Prune as CronEntry.Prune
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Action.Exception.Log as Exception.Log
import qualified Monadoc.Action.Exception.NotifySentry as Exception.NotifySentry
import qualified Monadoc.Action.Job.Acquire as Job.Acquire
import qualified Monadoc.Action.Job.Perform as Job.Perform
import qualified Monadoc.Action.Job.Release as Job.Release
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Extra.Exception as Exception
import qualified Monadoc.Type.App as App

worker :: App.App ()
worker = do
  mapM_ CronEntry.Upsert.run CronEntry.all
  CronEntry.Prune.run

  Monad.forever $ do
    CronEntry.Enqueue.run
    Exception.handleIf (not . Exception.isAsync) onException
      . Monad.void
      $ Exception.generalBracket Job.Acquire.run Job.Release.run Job.Perform.run

onException :: Exception.SomeException -> App.App ()
onException exception = do
  Exception.Log.run exception
  Exception.NotifySentry.run id exception
