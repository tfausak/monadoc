module Monadoc.Action.Exception.Log where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Type.App as App

run :: Exception.SomeException -> App.App ()
run = Log.error . Text.pack . Exception.displayException
