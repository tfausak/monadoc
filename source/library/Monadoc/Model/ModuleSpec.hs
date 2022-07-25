module Monadoc.Model.ModuleSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Module" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Module.Module
        { Module.name = Witch.unsafeFrom @String "Ex.Mod"
        }
      [ Sql.SQLText "Ex.Mod"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @Module.Model)
