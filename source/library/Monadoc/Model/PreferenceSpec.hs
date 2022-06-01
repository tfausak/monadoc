{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.PreferenceSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Preference" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Preference.Preference
        { Preference.package = Witch.from @Int 1,
          Preference.range = Witch.from @Int 2
        }
      [ Sql.SQLInteger 1,
        Sql.SQLInteger 2
      ]
