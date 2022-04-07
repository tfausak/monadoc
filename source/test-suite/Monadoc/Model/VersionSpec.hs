module Monadoc.Model.VersionSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Version" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Version.Version {Version.number = VersionNumber.zero}
      [Sql.SQLText "0"]
