module Monadoc.Action.Exception.Log where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Action.App.Log as App.Log
import qualified Monadoc.Type.App as App

run :: Exception.SomeException -> App.App ()
run = App.Log.error . Text.pack . Exception.displayException
