module Monadoc.Action.HackageIndex.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Type.Context as Context

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
  hackageIndex <- Maybe.listToMaybe <$> MonadSql.query_ "select key, size from hackageIndex order by key asc limit 1"
  maybe Insert.run (uncurry Update.run) hackageIndex
