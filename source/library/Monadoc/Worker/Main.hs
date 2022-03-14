module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Action.HackageIndex.Process as HackageIndex.Process
import qualified Monadoc.Action.HackageIndex.Upsert as HackageIndex.Upsert
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context

worker :: Context.Context -> IO ()
worker = App.run $ do
  App.say "updating index" -- TODO
  hackageIndex <- HackageIndex.Upsert.run
  App.say "processing index" -- TODO
  HackageIndex.Process.run hackageIndex
  App.say "done" -- TODO
  Monad.forever . App.lift $ Concurrent.threadDelay 1000000
