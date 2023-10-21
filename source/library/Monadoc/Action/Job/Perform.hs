module Monadoc.Action.Job.Perform where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Control as Control
import qualified Formatting as F
import qualified GHC.Clock as Clock
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.Task.Perform as Task.Perform
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: Maybe Job.Model -> App.App ()
run maybeJob = case maybeJob of
  Nothing -> IO.liftIO $ Concurrent.threadDelay 1_000_000
  Just job -> do
    let task = job.value.task
    App.Log.info $
      F.sformat
        ("[worker] starting" F.%+ Key.format F.% ":" F.%+ F.shown)
        job.key
        task
    before <- IO.liftIO Clock.getMonotonicTime
    () <- Control.control $ \runInBase ->
      Async.withAsync (runInBase $ Task.Perform.run job.value.task) Async.wait
    after <- IO.liftIO Clock.getMonotonicTime
    App.Log.info $
      F.sformat
        ("[worker] finished" F.%+ Key.format F.% ":" F.%+ F.shown F.%+ "in" F.%+ F.fixed 3)
        job.key
        task
        (after - before)
