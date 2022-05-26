{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.CronEntrySpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.CronEntry" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectByGuid" $ do
    Hspec.it "returns nothing when the cron entry doesn't exist" . Test.runFake $ do
      guid <- Test.arbitrary
      result <- CronEntry.selectByGuid guid
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the cron entry exists" . Test.runFake $ do
      guid <- Test.arbitrary
      cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectByGuid guid
      Base.liftBase $ result `Hspec.shouldBe` Just model
