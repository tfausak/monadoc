{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.KeySpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Key as Key
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Key" $ do
  Hspec.it "can be converted from an int" $ do
    Test.expectFrom (0 :: Int) Key.zero

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField Key.zero $ Sql.SQLInteger 0

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @(Key.Key ()))
