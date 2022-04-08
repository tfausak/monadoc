module Monadoc.Model.PreferenceSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Constraint as Constraint
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Preference" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Preference.Preference
        { Preference.constraint = Constraint.any,
          Preference.package = Witch.from @Int 1
        }
      [ Sql.SQLText ">=0",
        Sql.SQLInteger 1
      ]
