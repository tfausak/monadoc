module Monadoc.Model.DependencySpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Dependency as Dependency
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Dependency" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Dependency.Dependency
        { Dependency.packageMetaComponent = Witch.from @Int 1,
          Dependency.package = Witch.from @Int 2,
          Dependency.component = Witch.from @Int 3,
          Dependency.range = Witch.from @Int 4
        }
      [ Sql.SQLInteger 1,
        Sql.SQLInteger 2,
        Sql.SQLInteger 3,
        Sql.SQLInteger 4
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @Dependency.Model)
