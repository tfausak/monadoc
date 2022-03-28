module Monadoc.Exception.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql

data MigrationMismatch
  = MigrationMismatch Time.UTCTime Sql.Query Sql.Query
  deriving (Eq, Show)

instance Exception.Exception MigrationMismatch
