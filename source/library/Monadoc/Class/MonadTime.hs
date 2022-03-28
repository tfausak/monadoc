module Monadoc.Class.MonadTime where

import qualified Data.Time as Time
import qualified GHC.Clock as Clock

class Monad m => MonadTime m where
  getCurrentTime :: m Time.UTCTime
  getMonotonicTime :: m Double

instance MonadTime IO where
  getCurrentTime = Time.getCurrentTime
  getMonotonicTime = Clock.getMonotonicTime
