{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Model.JobSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Status as Status
import qualified Monadoc.Type.Task as Task
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Model.Job" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Job.Job
        { Job.createdAt = Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.007,
          Job.finishedAt = Just . Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.008,
          Job.startedAt = Just . Witch.from $ Time.makeUtcTime 2001 2 3 4 5 6.009,
          Job.status = Status.Passed,
          Job.task = Task.Vacuum
        }
      [ Sql.SQLText "2001-02-03 04:05:06.007",
        Sql.SQLText "2001-02-03 04:05:06.008",
        Sql.SQLText "2001-02-03 04:05:06.009",
        Sql.SQLText "Passed",
        Sql.SQLBlob "{\"tag\":\"Vacuum\"}"
      ]

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlRow @Job.Model)
