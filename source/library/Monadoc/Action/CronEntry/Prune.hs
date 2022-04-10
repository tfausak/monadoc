module Monadoc.Action.CronEntry.Prune where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Witch

run :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => m ()
run = do
  let toKeep = Set.fromList $ Maybe.mapMaybe CronEntry.guid CronEntry.all
  guids <- MonadSql.query_ "select guid from cronEntry where guid is not null"
  Monad.forM_ guids $ \(Sql.Only guid) ->
    Monad.when (Set.notMember guid toKeep) $ do
      MonadSql.execute "delete from cronEntry where guid = ?" [guid]
      MonadLog.debug $ "deleted static cron entry: " <> Witch.from guid
