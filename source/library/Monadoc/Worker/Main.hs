{-# LANGUAGE TypeApplications #-}

module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.Witch as Witch

worker :: Context.Context -> IO ()
worker = App.run $ do
  App.sayString "vacuuming"
  App.withConnection $ \connection ->
    App.lift . Sql.execute_ connection $ Witch.into @Sql.Query "pragma incremental_vacuum"
  App.sayString "upserting hackage index"
  hackageIndex <- HackageIndex.Upsert.run
  App.sayString "processing hackage index"
  HackageIndex.Process.run hackageIndex
  App.sayString "done working"
  Monad.forever . App.lift $ Concurrent.threadDelay 1000000
