{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Prune where

import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Formatting as F
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Action.CronEntry.Delete as CronEntry.Delete
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model
import qualified Witch

run :: App.App ()
run = do
  let toKeep = Set.fromList $ Maybe.mapMaybe CronEntry.guid CronEntry.all
  cronEntries <- CronEntry.selectWithGuid
  Monad.forM_ cronEntries $ \cronEntry ->
    case CronEntry.guid $ Model.value cronEntry of
      Nothing -> pure ()
      Just guid -> Monad.when (Set.notMember guid toKeep) $ do
        CronEntry.Delete.run guid
        App.Log.debug $ F.sformat ("deleted static cron entry: " F.% F.stext) (Witch.from guid)
