{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.CronEntry.EnqueueSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Monadoc.Action.CronEntry.Enqueue as CronEntry.Enqueue
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry
import qualified Monadoc.Query.Job as Job
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Enqueue" . Hspec.around Test.withConnection $ do
  Hspec.it "succeeds with no cron entries" . Test.runFake $ do
    CronEntry.Enqueue.run

  Hspec.it "updates the cron entry's next run at" . Test.runFake $ do
    now <- Timestamp.getCurrentTime
    schedule <- either Exception.throwM pure $ Witch.tryFrom @Text.Text "* * * * *"
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.runAt = now, CronEntry.schedule = schedule}
    model <- CronEntry.Insert.run cronEntry
    CronEntry.Enqueue.run
    result <- CronEntry.selectByKey $ Model.key model
    Base.liftBase $
      result
        `Hspec.shouldBe` Just
          model
            { Model.value = cronEntry {CronEntry.runAt = nextMinute now}
            }

  Hspec.it "inserts a job" . Test.runFake $ do
    now <- Timestamp.getCurrentTime
    schedule <- either Exception.throwM pure $ Witch.tryFrom @Text.Text "* * * * *"
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.runAt = now, CronEntry.schedule = schedule}
    Monad.void $ CronEntry.Insert.run cronEntry
    CronEntry.Enqueue.run
    result <- Job.selectAll
    Base.liftBase $ result `Hspec.shouldNotBe` []

nextMinute :: Timestamp.Timestamp -> Timestamp.Timestamp
nextMinute = Witch.over @Time.UTCTime $ \t ->
  Time.addUTCTime
    60
    t
      { Time.utctDayTime =
          let scale = 60000000000000 :: Integer
           in Time.picosecondsToDiffTime
                . (*) scale
                . flip div scale
                . Time.diffTimeToPicoseconds
                $ Time.utctDayTime t
      }
