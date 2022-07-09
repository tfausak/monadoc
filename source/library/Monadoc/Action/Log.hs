{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Log where

import qualified Control.Monad.Trans.Class as Trans
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Severity as Severity
import qualified Say
import qualified System.IO as IO
import qualified Witch

run :: Severity.Severity -> Text.Text -> App.App ()
run severity message = do
  let handle = if severity >= Severity.Warn then IO.stderr else IO.stdout
  now <- Trans.lift Time.getCurrentTime
  let timestamp = Text.pack $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now
  Trans.lift . Say.hSay handle $
    Text.unwords
      [ timestamp,
        Text.pack $ "[" <> Witch.into @String severity <> "]",
        message
      ]

debug :: Text.Text -> App.App ()
debug = run Severity.Debug

info :: Text.Text -> App.App ()
info = run Severity.Info

warn :: Text.Text -> App.App ()
warn = run Severity.Warn

error :: Text.Text -> App.App ()
error = run Severity.Error
