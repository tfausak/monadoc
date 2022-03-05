module Monadoc.Worker where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Monadoc.Type.Context as Context

worker :: Context.Context -> IO ()
worker = const . Monad.forever $ Concurrent.threadDelay 1000000
