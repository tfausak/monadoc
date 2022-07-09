{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Query.CronEntrySpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Data.Function as Function
import qualified Data.Ord as Ord
import qualified Data.Time as Time
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.CronEntry" $ do
  Hspec.describe "selectAll" $ do
    Hspec.it "works when there are no cron entries" . Test.run $ do
      cronEntries <- CronEntry.selectAll
      Base.liftBase $ cronEntries `Hspec.shouldBe` []

    Hspec.it "works when there are cron entries" . Test.run $ do
      cronEntry <- do
        x <- Test.arbitrary
        CronEntry.Insert.run x
      cronEntries <- CronEntry.selectAll
      Base.liftBase $ cronEntries `Hspec.shouldBe` [cronEntry]

  Hspec.describe "selectByGuid" $ do
    Hspec.it "returns nothing when the cron entry doesn't exist" . Test.run $ do
      guid <- Test.arbitrary
      result <- CronEntry.selectByGuid guid
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the cron entry exists" . Test.run $ do
      guid <- Test.arbitrary
      cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectByGuid guid
      Base.liftBase $ result `Hspec.shouldBe` Just model

  Hspec.describe "selectByKey" $ do
    Hspec.it "returns nothing when the cron entry doesn't exist" . Test.run $ do
      key <- Test.arbitrary
      result <- CronEntry.selectByKey key
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns just when the cron entry exists" . Test.run $ do
      cronEntry <- Test.arbitrary
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectByKey $ Model.key model
      Base.liftBase $ result `Hspec.shouldBe` Just model

  Hspec.describe "selectNext" $ do
    Hspec.it "returns nothing when there are no cron entries" . Test.run $ do
      now <- Timestamp.getCurrentTime
      result <- CronEntry.selectNext now
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a cron entry before the time" . Test.run $ do
      cronEntry <- Test.arbitrary
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectNext . Witch.over @Time.UTCTime (Time.addUTCTime 1) $ CronEntry.runAt cronEntry
      Base.liftBase $ result `Hspec.shouldBe` Just model

    Hspec.it "returns a cron entry at the time" . Test.run $ do
      cronEntry <- Test.arbitrary
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectNext $ CronEntry.runAt cronEntry
      Base.liftBase $ result `Hspec.shouldBe` Just model

    Hspec.it "does not return a cron entry after the time" . Test.run $ do
      cronEntry <- Test.arbitrary
      Monad.void $ CronEntry.Insert.run cronEntry
      result <- CronEntry.selectNext . Witch.over @Time.UTCTime (Time.addUTCTime (-1)) $ CronEntry.runAt cronEntry
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "sorts cron entries by time" . Test.run $ do
      cronEntry1 <- Test.arbitrary
      cronEntry2 <- Test.arbitrary
      model1 <- CronEntry.Insert.run cronEntry1
      model2 <- CronEntry.Insert.run cronEntry2
      result <- CronEntry.selectNext $ Function.on max CronEntry.runAt cronEntry1 cronEntry2
      let expected = minOn (CronEntry.runAt . Model.value) model1 model2
      Base.liftBase $ result `Hspec.shouldBe` Just expected

  Hspec.describe "selectWithGuid" $ do
    Hspec.it "returns an empty list when there are no cron entries" . Test.run $ do
      result <- CronEntry.selectWithGuid
      Base.liftBase $ result `Hspec.shouldBe` []

    Hspec.it "returns a cron entry with a guid" . Test.run $ do
      guid <- Test.arbitrary
      cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
      model <- CronEntry.Insert.run cronEntry
      result <- CronEntry.selectWithGuid
      Base.liftBase $ result `Hspec.shouldBe` [model]

    Hspec.it "does not return a cron entry without a guid" . Test.run $ do
      cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Nothing}
      Monad.void $ CronEntry.Insert.run cronEntry
      result <- CronEntry.selectWithGuid
      Base.liftBase $ result `Hspec.shouldBe` []

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy f x y = case f x y of
  GT -> y
  _ -> x

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn f = minBy $ Ord.comparing f
