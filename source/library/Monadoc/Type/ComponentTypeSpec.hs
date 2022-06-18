{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentTypeSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.ComponentType as ComponentType
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.ComponentType" $ do
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("bench" :: String) ComponentType.Benchmark
    Test.expectTryFrom ("exe" :: String) ComponentType.Executable
    Test.expectTryFrom ("flib" :: String) ComponentType.ForeignLibrary
    Test.expectTryFrom ("lib" :: String) ComponentType.Library
    Test.expectTryFrom ("test" :: String) ComponentType.TestSuite

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom ComponentType.Benchmark ("bench" :: String)
    Test.expectFrom ComponentType.Executable ("exe" :: String)
    Test.expectFrom ComponentType.ForeignLibrary ("flib" :: String)
    Test.expectFrom ComponentType.Library ("lib" :: String)
    Test.expectFrom ComponentType.TestSuite ("test" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField ComponentType.Benchmark $ Sql.SQLText "bench"
    Test.expectSqlField ComponentType.Executable $ Sql.SQLText "exe"
    Test.expectSqlField ComponentType.ForeignLibrary $ Sql.SQLText "flib"
    Test.expectSqlField ComponentType.Library $ Sql.SQLText "lib"
    Test.expectSqlField ComponentType.TestSuite $ Sql.SQLText "test"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @ComponentType.ComponentType)
