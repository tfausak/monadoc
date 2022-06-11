{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.UpsertSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a dynamic cron entry" . Test.runFake $ do
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Nothing}
    Monad.void $ CronEntry.Upsert.run cronEntry
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry]

  Hspec.it "duplicates a dynamic cron entry" . Test.runFake $ do
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Nothing}
    Monad.void $ CronEntry.Upsert.run cronEntry
    Monad.void $ CronEntry.Upsert.run cronEntry
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry, cronEntry]

  Hspec.it "inserts a static cron entry" . Test.runFake $ do
    guid <- Test.arbitrary
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    Monad.void $ CronEntry.Upsert.run cronEntry
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry]

  Hspec.it "does not duplicate a static cron entry" . Test.runFake $ do
    guid <- Test.arbitrary
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    Monad.void $ CronEntry.Upsert.run cronEntry
    Monad.void $ CronEntry.Upsert.run cronEntry
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry]

  Hspec.it "updates a static cron entry" . Test.runFake $ do
    guid <- Test.arbitrary
    cronEntry1 <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    cronEntry2 <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    Monad.void $ CronEntry.Upsert.run cronEntry1
    Monad.void $ CronEntry.Upsert.run cronEntry2
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry2]

  Hspec.it "does not update a static cron entry with the same schedule and task" . Test.runFake $ do
    guid <- Test.arbitrary
    cronEntry1 <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    cronEntry2 <- Test.arbitraryWith $ \x ->
      x
        { CronEntry.guid = Just guid,
          CronEntry.schedule = CronEntry.schedule cronEntry1,
          CronEntry.task = CronEntry.task cronEntry1
        }
    Monad.void $ CronEntry.Upsert.run cronEntry1
    Monad.void $ CronEntry.Upsert.run cronEntry2
    cronEntries <- CronEntry.selectAll
    Base.liftBase $ fmap Model.value cronEntries `Hspec.shouldBe` [cronEntry1]
