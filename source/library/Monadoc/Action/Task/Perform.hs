module Monadoc.Action.Task.Perform where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Prune as HackageIndex.Prune
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Task as Task

run ::
  ( Control.MonadBaseControl IO m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m,
    MonadHttp.MonadHttp m
  ) =>
  Task.Task ->
  m ()
run task = case task of
  Task.ProcessHackageIndex -> HackageIndex.Process.run
  Task.PruneHackageIndex -> HackageIndex.Prune.run
  Task.UpsertHackageIndex -> HackageIndex.Upsert.run
  Task.Vacuum -> Database.Vacuum.run
