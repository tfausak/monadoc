module Monadoc.Class.MonadSay where

import qualified Data.Text as Text
import qualified Say
import qualified System.IO as IO

class Monad m => MonadSay m where
  hSay :: IO.Handle -> Text.Text -> m ()

instance MonadSay IO where
  hSay = Say.hSay
