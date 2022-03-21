{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Task where

import qualified Data.Aeson as Aeson
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Vendor.Witch as Witch

data Task
  = ProcessHackageIndex
  | UpsertHackageIndex
  | Vacuum
  deriving (Eq, Show)

instance Aeson.FromJSON Task where
  parseJSON = Aeson.withObject "Task" $ \object -> do
    tag <- object Aeson..: Witch.into @Aeson.Key "tag"
    case tag of
      "ProcessHackageIndex" -> pure ProcessHackageIndex
      "UpsertHackageIndex" -> pure UpsertHackageIndex
      "Vacuum" -> pure Vacuum
      _ -> fail $ "unknown tag: " <> show tag

instance Aeson.ToJSON Task where
  toJSON task = case task of
    ProcessHackageIndex -> Aeson.object [Witch.into @Aeson.Key "tag" Aeson..= "ProcessHackageIndex"]
    UpsertHackageIndex -> Aeson.object [Witch.into @Aeson.Key "tag" Aeson..= "UpsertHackageIndex"]
    Vacuum -> Aeson.object [Witch.into @Aeson.Key "tag" Aeson..= "Vacuum"]

instance Sql.FromField Task where
  fromField field = do
    byteString <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Aeson.eitherDecode byteString

instance Sql.ToField Task where
  toField = Sql.toField . Aeson.encode
