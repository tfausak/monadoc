module Monadoc.Exception.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Data.Time as Time

data MigrationMismatch
  = MigrationMismatch Time.UTCTime Text.Text Text.Text
  deriving (Eq, Show)

instance Exception.Exception MigrationMismatch
