module Monadoc.Type.Task where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.QuickCheck as QuickCheck

data Task
  = ProcessHackageIndex
  | ProcessUploads
  | PruneHackageIndex
  | PruneJobs
  | UpsertHackageIndex
  | Vacuum
  deriving (Eq, Show)

instance Aeson.FromJSON Task where
  parseJSON = Aeson.withObject "Task" $ \object -> do
    tag <- object Aeson..: "tag"
    case tag :: Text.Text of
      "ProcessHackageIndex" -> pure ProcessHackageIndex
      "ProcessUploads" -> pure ProcessUploads
      "PruneHackageIndex" -> pure PruneHackageIndex
      "PruneJobs" -> pure PruneJobs
      "UpsertHackageIndex" -> pure UpsertHackageIndex
      "Vacuum" -> pure Vacuum
      _ -> fail $ "unknown tag: " <> show tag

instance Aeson.ToJSON Task where
  toJSON task =
    let tag = case task of
          ProcessHackageIndex -> "ProcessHackageIndex"
          ProcessUploads -> "ProcessUploads"
          PruneHackageIndex -> "PruneHackageIndex"
          PruneJobs -> "PruneJobs"
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

instance QuickCheck.Arbitrary Task where
  arbitrary =
    QuickCheck.elements
      [ ProcessHackageIndex,
        ProcessUploads,
        PruneHackageIndex,
        PruneJobs,
        UpsertHackageIndex,
        Vacuum
      ]
