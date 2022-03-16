module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context

worker :: Context.Context -> IO ()
worker = App.run $ do
  hackageIndex <- HackageIndex.Upsert.run
  HackageIndex.Process.run hackageIndex
  Monad.forever . App.lift $ Concurrent.threadDelay 1000000
