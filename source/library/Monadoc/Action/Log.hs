{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Log where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Text as Text
import qualified Formatting as F
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Severity as Severity
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Say
import qualified System.IO as IO
import qualified Witch

run :: Severity.Severity -> Text.Text -> App.App ()
run severity message = do
  context <- Reader.ask
  Monad.when (severity >= Config.severity (Context.config context)) $ do
    let handle = if severity >= Severity.Warn then IO.stderr else IO.stdout
    timestamp <- Timestamp.getCurrentTime
    IO.liftIO . Say.hSay handle $
      F.sformat
        (F.stext F.% " [" F.% F.string F.% "] " F.% F.stext)
        (Witch.from timestamp)
        (Witch.from severity)
        message

debug :: Text.Text -> App.App ()
debug = run Severity.Debug

info :: Text.Text -> App.App ()
info = run Severity.Info

warn :: Text.Text -> App.App ()
warn = run Severity.Warn

error :: Text.Text -> App.App ()
error = run Severity.Error
