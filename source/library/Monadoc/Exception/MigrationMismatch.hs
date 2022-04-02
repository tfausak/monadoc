module Monadoc.Exception.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Timestamp as Timestamp

data MigrationMismatch = MigrationMismatch
  { createdAt :: Timestamp.Timestamp,
    expected :: Sql.Query,
    actual :: Sql.Query
  }
  deriving (Eq, Show)

instance Exception.Exception MigrationMismatch
