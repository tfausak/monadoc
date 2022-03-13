module Monadoc.Worker.Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Action.UpdateIndex as UpdateIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context

worker :: Context.Context -> IO ()
worker = App.run $ do
  UpdateIndex.run
  Monad.forever . App.lift $ Concurrent.threadDelay 1000000
