{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: App.App ()
run = do
  rows <- App.query_ "select * from hackageIndex where processedAt is not null order by processedAt desc"
  case rows of
    [] -> Log.warn "no hackage indexes to prune"
    keep : rest -> do
      Log.info $ "keeping hackage index: " <> Witch.into @Text.Text (Model.key keep)
      Log.debug $ "pruning hackage indexes: " <> (Witch.into @Text.Text . show $ length rest)
      Monad.forM_ rest $ \hackageIndex -> do
        Log.debug $ "pruning hackage index: " <> Witch.into @Text.Text (Model.key hackageIndex)
        App.execute "delete from hackageIndex where key = ?" [Model.key hackageIndex]
        App.execute "delete from blob where key = ?" [HackageIndex.blob $ Model.value hackageIndex]
