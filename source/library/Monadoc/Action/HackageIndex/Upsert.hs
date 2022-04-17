module Monadoc.Action.HackageIndex.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Timestamp as Timestamp

run ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m
  ) =>
  m ()
run = do
  MonadLog.debug "upserting hackage index"
  rows <- MonadSql.query_ "select key, size, updatedAt from hackageIndex order by createdAt desc limit 1"
  case rows of
    [] -> Insert.run
    (key, size, maybeUpdatedAt) : _ -> case maybeUpdatedAt :: Maybe Timestamp.Timestamp of
      Nothing -> MonadLog.warn "previous insert or update is not yet finished"
      Just _ -> Update.run key size
