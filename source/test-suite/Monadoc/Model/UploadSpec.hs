module Monadoc.Model.UploadSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Upload" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Upload.Upload
        { Upload.blob = Witch.from @Int 1,
          Upload.package = Witch.from @Int 2,
          Upload.revision = Witch.from @Word 3,
          Upload.uploadedAt = Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          Upload.uploadedBy = Witch.from @Int 4,
          Upload.version = Witch.from @Int 5
        }
      [ Sql.SQLInteger 1,
        Sql.SQLInteger 2,
        Sql.SQLInteger 3,
        Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLInteger 4,
        Sql.SQLInteger 5
      ]
