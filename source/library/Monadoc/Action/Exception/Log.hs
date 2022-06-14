module Monadoc.Action.Exception.Log where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Monadoc.Class.MonadLog as MonadLog

run :: MonadLog.MonadLog m => Exception.SomeException -> m ()
run = MonadLog.error . Text.pack . Exception.displayException
