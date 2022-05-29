module Monadoc.Exception.MigrationMismatch where

import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Timestamp as Timestamp

data MigrationMismatch = MigrationMismatch
  { createdAt :: Timestamp.Timestamp,
    expected :: Query.Query,
    actual :: Query.Query
  }
  deriving (Eq, Show)

instance Exception.Exception MigrationMismatch
