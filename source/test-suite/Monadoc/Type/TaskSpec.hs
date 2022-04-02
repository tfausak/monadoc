module Monadoc.Type.TaskSpec where

import qualified Data.Aeson as Aeson
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.Internal as Sql
import qualified Database.SQLite.Simple.Ok as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Task as Task
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Task" $ do
  Hspec.it "can be converted to JSON" $ do
    Aeson.encode Task.ProcessHackageIndex `Hspec.shouldBe` "{\"tag\":\"ProcessHackageIndex\"}"
    Aeson.encode Task.UpsertHackageIndex `Hspec.shouldBe` "{\"tag\":\"UpsertHackageIndex\"}"
    Aeson.encode Task.Vacuum `Hspec.shouldBe` "{\"tag\":\"Vacuum\"}"
  Hspec.it "can be converted from JSON" $ do
    Aeson.decode "{\"tag\":\"ProcessHackageIndex\"}" `Hspec.shouldBe` Just Task.ProcessHackageIndex
    Aeson.decode "{\"tag\":\"UpsertHackageIndex\"}" `Hspec.shouldBe` Just Task.UpsertHackageIndex
    Aeson.decode "{\"tag\":\"Vacuum\"}" `Hspec.shouldBe` Just Task.Vacuum
  Hspec.it "can be converted to SQL" $ do
    Sql.toField Task.ProcessHackageIndex `Hspec.shouldBe` Sql.SQLBlob "{\"tag\":\"ProcessHackageIndex\"}"
    Sql.toField Task.UpsertHackageIndex `Hspec.shouldBe` Sql.SQLBlob "{\"tag\":\"UpsertHackageIndex\"}"
    Sql.toField Task.Vacuum `Hspec.shouldBe` Sql.SQLBlob "{\"tag\":\"Vacuum\"}"
  Hspec.it "can be converted from SQL" $ do
    Sql.fromField (Sql.Field (Sql.SQLBlob "{\"tag\":\"ProcessHackageIndex\"}") 0) `Hspec.shouldBe` Sql.Ok Task.ProcessHackageIndex
    Sql.fromField (Sql.Field (Sql.SQLBlob "{\"tag\":\"UpsertHackageIndex\"}") 0) `Hspec.shouldBe` Sql.Ok Task.UpsertHackageIndex
    Sql.fromField (Sql.Field (Sql.SQLBlob "{\"tag\":\"Vacuum\"}") 0) `Hspec.shouldBe` Sql.Ok Task.Vacuum
