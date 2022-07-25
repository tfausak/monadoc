module Monadoc.Model.BlobSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Blob" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Blob.Blob
        { Blob.contents = "example",
          Blob.hash = Hash.new mempty,
          Blob.size = 7
        }
      [ Sql.SQLInteger 7,
        Sql.SQLBlob "\xe3\xb0\xc4\x42\x98\xfc\x1c\x14\x9a\xfb\xf4\xc8\x99\x6f\xb9\x24\x27\xae\x41\xe4\x64\x9b\x93\x4c\xa4\x95\x99\x1b\x78\x52\xb8\x55",
        Sql.SQLBlob "example"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @Blob.Model)
