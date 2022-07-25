module Monadoc.Type.TaskSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Task as Task
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Task" $ do
  Hspec.it "can be round-tripped through JSON" $ do
    Test.expectJson Task.ProcessHackageIndex "{\"tag\":\"ProcessHackageIndex\"}"
    Test.expectJson Task.ProcessUploads "{\"tag\":\"ProcessUploads\"}"
    Test.expectJson Task.PruneHackageIndex "{\"tag\":\"PruneHackageIndex\"}"
    Test.expectJson Task.UpsertHackageIndex "{\"tag\":\"UpsertHackageIndex\"}"
    Test.expectJson Task.Vacuum "{\"tag\":\"Vacuum\"}"

  Hspec.it "can be round-tripped through JSON" $
    QuickCheck.property (Test.propertyJson @Task.Task)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField Task.ProcessHackageIndex $ Sql.SQLBlob "{\"tag\":\"ProcessHackageIndex\"}"
    Test.expectSqlField Task.ProcessUploads $ Sql.SQLBlob "{\"tag\":\"ProcessUploads\"}"
    Test.expectSqlField Task.PruneHackageIndex $ Sql.SQLBlob "{\"tag\":\"PruneHackageIndex\"}"
    Test.expectSqlField Task.UpsertHackageIndex $ Sql.SQLBlob "{\"tag\":\"UpsertHackageIndex\"}"
    Test.expectSqlField Task.Vacuum $ Sql.SQLBlob "{\"tag\":\"Vacuum\"}"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Task.Task)
