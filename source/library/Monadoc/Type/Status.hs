module Monadoc.Type.Status where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Witch

data Status
  = Failed
  | Locked
  | Queued
  | Passed
  deriving (Eq, Show)

instance Witch.TryFrom String Status where
  tryFrom = Witch.maybeTryFrom $ \string -> case string of
    "Failed" -> Just Failed
    "Locked" -> Just Locked
    "Queued" -> Just Queued
    "Passed" -> Just Passed
    _ -> Nothing

instance Witch.From Status String where
  from status = case status of
    Failed -> "Failed"
    Locked -> "Locked"
    Queued -> "Queued"
    Passed -> "Passed"

instance Sql.FromField Status where
  fromField field = do
    string <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom @String string

instance Sql.ToField Status where
  toField = Sql.toField . Witch.into @String
