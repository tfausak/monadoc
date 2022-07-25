module Monadoc.Model.PackageMetaSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.Hash as Hash
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.PackageMeta" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    let hash = Hash.new ""
    Test.expectSqlRow
      PackageMeta.PackageMeta
        { PackageMeta.buildType = BuildType.simple,
          PackageMeta.cabalVersion = Witch.from @Int 1,
          PackageMeta.hash = hash,
          PackageMeta.license = Witch.from @Int 2,
          PackageMeta.upload = Witch.from @Int 3,
          PackageMeta.author = Just "a",
          PackageMeta.bugReports = Just "b",
          PackageMeta.category = Just "c",
          PackageMeta.copyright = Just "d",
          PackageMeta.description = Just "e",
          PackageMeta.homepage = Just "f",
          PackageMeta.maintainer = Just "g",
          PackageMeta.pkgUrl = Just "h",
          PackageMeta.stability = Just "i",
          PackageMeta.synopsis = Just "j"
        }
      [ Sql.SQLText "Simple",
        Sql.SQLInteger 1,
        Sql.SQLBlob $ Witch.from hash,
        Sql.SQLInteger 2,
        Sql.SQLInteger 3,
        Sql.SQLText "a",
        Sql.SQLText "b",
        Sql.SQLText "c",
        Sql.SQLText "d",
        Sql.SQLText "e",
        Sql.SQLText "f",
        Sql.SQLText "g",
        Sql.SQLText "h",
        Sql.SQLText "i",
        Sql.SQLText "j"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @PackageMeta.Model)
