{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.DeleteSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.CronEntry.Delete as CronEntry.Delete
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Delete" . Hspec.around Test.withConnection $ do
  Hspec.it "does not throw an error when the cron entry doesn't exist" . Test.runFake $ do
    guid <- Test.arbitrary
    CronEntry.Delete.run guid

  Hspec.it "deletes the cron entry" . Test.runFake $ do
    guid <- Test.arbitrary
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    Monad.void $ CronEntry.Insert.run cronEntry
    CronEntry.Delete.run guid
    maybeModel <- CronEntry.selectByGuid guid
    Base.liftBase $ maybeModel `Hspec.shouldBe` Nothing
