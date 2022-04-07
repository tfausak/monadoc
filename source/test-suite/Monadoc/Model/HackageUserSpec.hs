module Monadoc.Model.HackageUserSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.HackageUser" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      HackageUser.HackageUser {HackageUser.name = Witch.from @String "example"}
      [Sql.SQLText "example"]
