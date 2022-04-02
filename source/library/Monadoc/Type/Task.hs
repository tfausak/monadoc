module Monadoc.Type.Task where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql

data Task
  = ProcessHackageIndex
  | UpsertHackageIndex
  | Vacuum
  deriving (Eq, Show)

instance Aeson.FromJSON Task where
  parseJSON = Aeson.withObject "Task" $ \object -> do
    tag <- object Aeson..: "tag"
    case tag :: Text.Text of
      "ProcessHackageIndex" -> pure ProcessHackageIndex
      "UpsertHackageIndex" -> pure UpsertHackageIndex
      "Vacuum" -> pure Vacuum
      _ -> fail $ "unknown tag: " <> show tag

instance Aeson.ToJSON Task where
  toJSON task =
    let tag = case task of
          ProcessHackageIndex -> "ProcessHackageIndex"
          UpsertHackageIndex -> "UpsertHackageIndex"
          Vacuum -> "Vacuum" :: Text.Text
     in Aeson.object ["tag" Aeson..= tag]

instance Sql.FromField Task where
  fromField field = do
    byteString <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Aeson.eitherDecode byteString

instance Sql.ToField Task where
  toField = Sql.toField . Aeson.encode
