module Monadoc.Model.PackageMetaComponentModuleSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.PackageMetaComponentModule as PackageMetaComponentModule
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.PackageMetaComponentModule" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      PackageMetaComponentModule.PackageMetaComponentModule
        { PackageMetaComponentModule.packageMetaComponent = Witch.from @Int 1,
          PackageMetaComponentModule.module_ = Witch.from @Int 2
        }
      [ Sql.SQLInteger 1,
        Sql.SQLInteger 2
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @PackageMetaComponentModule.Model)
