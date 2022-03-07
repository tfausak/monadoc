{-# LANGUAGE TypeApplications #-}

module Monadoc.Class.MonadSay where

import qualified Data.Text as Text
import qualified Monadoc.Vendor.Witch as Witch
import qualified Say as Say
import qualified System.IO as IO

class Monad m => MonadSay m where
  hSay :: IO.Handle -> Text.Text -> m ()

instance MonadSay IO where
  hSay = Say.hSay

say :: MonadSay m => Text.Text -> m ()
say = hSay IO.stdout

sayErr :: MonadSay m => Text.Text -> m ()
sayErr = hSay IO.stderr

sayErrString :: MonadSay m => String -> m ()
sayErrString = sayErr . Witch.into @Text.Text

sayString :: MonadSay m => String -> m ()
sayString = say . Witch.into @Text.Text
