{-# LANGUAGE TypeApplications #-}

module Monadoc.Class.MonadLog where

import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Monadoc.Class.MonadSay as MonadSay
import qualified Monadoc.Class.MonadTime as MonadTime
import qualified Monadoc.Type.Severity as Severity
import qualified System.IO as IO
import qualified Witch

class (MonadSay.MonadSay m, MonadTime.MonadTime m) => MonadLog m where
  log :: Severity.Severity -> Text.Text -> m ()
  log severity message = do
    let handle = if severity >= Severity.Warn then IO.stderr else IO.stdout
    now <- MonadTime.getCurrentTime
    let timestamp = Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now
    MonadSay.hSay handle $
      Text.unwords
        [ timestamp,
          Text.pack $ "[" <> Witch.into @String severity <> "]",
          message
        ]

instance MonadLog IO

debug :: MonadLog m => Text.Text -> m ()
debug = Monadoc.Class.MonadLog.log Severity.Debug

info :: MonadLog m => Text.Text -> m ()
info = Monadoc.Class.MonadLog.log Severity.Info

warn :: MonadLog m => Text.Text -> m ()
warn = Monadoc.Class.MonadLog.log Severity.Warn

error :: MonadLog m => Text.Text -> m ()
error = Monadoc.Class.MonadLog.log Severity.Error
