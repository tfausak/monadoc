module Monadoc.Class.MonadSleep where

import qualified Control.Concurrent as Concurrent

class Monad m => MonadSleep m where
  sleep :: Double -> m ()

instance MonadSleep IO where
  sleep = Concurrent.threadDelay . round . (*) 1e6
