{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: (MonadLog.MonadLog m, MonadSql.MonadSql m) => m ()
run = do
  rows <- MonadSql.query_ "select * from hackageIndex where processedAt is not null order by processedAt desc"
  case rows of
    [] -> MonadLog.warn "no hackage indexes to prune"
    keep : rest -> do
      MonadLog.info $ "keeping hackage index: " <> Witch.into @Text.Text (Model.key keep)
      MonadLog.debug $ "pruning hackage indexes: " <> (Witch.into @Text.Text . show $ length rest)
      Monad.forM_ rest $ \hackageIndex -> do
        MonadLog.debug $ "pruning hackage index: " <> Witch.into @Text.Text (Model.key hackageIndex)
        MonadSql.execute "delete from hackageIndex where key = ?" [Model.key hackageIndex]
        MonadSql.execute "delete from blob where key = ?" [HackageIndex.blob $ Model.value hackageIndex]
