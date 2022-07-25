module Monadoc.Type.SpdxSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Distribution.SPDX as Cabal
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Spdx as Spdx
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Spdx" $ do
  let spdx = Witch.into @Spdx.Spdx . Cabal.License $ Cabal.ELicense (Cabal.ELicenseId Cabal.MIT) Nothing
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("MIT" :: String) spdx

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom spdx ("MIT" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField spdx (Sql.SQLText "MIT")

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Spdx.Spdx)
