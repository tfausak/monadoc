module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Witch

run :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => m ()
run = do
  rows <- MonadSql.query_ "select key from hackageIndex where updatedAt is not null and processedAt is not null order by processedAt desc"
  case fmap Sql.fromOnly rows of
    [] -> MonadLog.warn "no hackage indexes to prune"
    keep : keys -> do
      MonadLog.info $ "keeping hackage index: " <> Witch.into @Text.Text (keep :: HackageIndex.Key)
      MonadLog.debug $ "pruning hackage indexes: " <> (Witch.into @Text.Text . show $ length keys)
      Monad.forM_ keys $ \key -> do
        MonadLog.debug $ "pruning hackage index: " <> Witch.into @Text.Text key
        MonadSql.execute "delete from hackageIndex where key = ?" [key]
