{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.PackageSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Package" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Package.Package
        { Package.name = Witch.unsafeFrom @String "example"
        }
      [ Sql.SQLText "example"
      ]
