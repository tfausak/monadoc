module Monadoc.Class.MonadFile where

import qualified Data.Time as Time
import qualified System.Directory as Directory

class Monad m => MonadFile m where
  getModificationTime :: FilePath -> m Time.UTCTime

instance MonadFile IO where
  getModificationTime = Directory.getModificationTime
