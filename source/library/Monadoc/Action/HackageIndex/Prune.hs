{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.HackageIndex.Prune where

import qualified Control.Monad as Monad
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

run :: App.App ()
run = do
  rows <- App.Sql.query_ "select * from hackageIndex where processedAt is not null order by processedAt desc"
  case rows of
    [] -> App.Log.warn "no hackage indexes to prune"
    keep : rest -> do
      App.Log.info $ F.sformat ("keeping " F.% Key.format) (Model.key keep)
      Monad.forM_ rest $ \hackageIndex -> do
        App.Log.debug $ F.sformat ("pruning " F.% Key.format) (Model.key hackageIndex)
        App.Sql.execute "delete from hackageIndex where key = ?" [Model.key hackageIndex]
        App.Sql.execute "delete from blob where key = ?" [HackageIndex.blob $ Model.value hackageIndex]
