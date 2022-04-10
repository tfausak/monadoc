module Monadoc.Exception.NoNextMatch where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Schedule as Schedule
import qualified Monadoc.Type.Timestamp as Timestamp

data NoNextMatch
  = NoNextMatch Timestamp.Timestamp Schedule.Schedule
  deriving (Eq, Show)

instance Exception.Exception NoNextMatch
