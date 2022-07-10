{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Formatting as F
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: App.App ()
run = do
  rows <- App.query_ "select * from hackageIndex where processedAt is not null order by processedAt desc"
  case rows of
    [] -> Log.warn "no hackage indexes to prune"
    keep : rest -> do
      Log.info $ F.sformat ("keeping " F.% Key.format) (Model.key keep)
      Monad.forM_ rest $ \hackageIndex -> do
        Log.debug $ F.sformat ("pruning " F.% Key.format) (Model.key hackageIndex)
        App.execute "delete from hackageIndex where key = ?" [Model.key hackageIndex]
        App.execute "delete from blob where key = ?" [HackageIndex.blob $ Model.value hackageIndex]
