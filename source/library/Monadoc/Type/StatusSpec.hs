{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Type.StatusSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Status as Status
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Status" $ do
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("Failed" :: String) Status.Failed
    Test.expectTryFrom ("Locked" :: String) Status.Locked
    Test.expectTryFrom ("Queued" :: String) Status.Queued
    Test.expectTryFrom ("Passed" :: String) Status.Passed

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom Status.Failed ("Failed" :: String)
    Test.expectFrom Status.Locked ("Locked" :: String)
    Test.expectFrom Status.Queued ("Queued" :: String)
    Test.expectFrom Status.Passed ("Passed" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField Status.Failed $ Sql.SQLText "Failed"
    Test.expectSqlField Status.Locked $ Sql.SQLText "Locked"
    Test.expectSqlField Status.Queued $ Sql.SQLText "Queued"
    Test.expectSqlField Status.Passed $ Sql.SQLText "Passed"
