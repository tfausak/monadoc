{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.CronEntry.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new cron entry" . Test.runFake $ do
    cronEntry <- Test.arbitrary
    actual <- CronEntry.Insert.run cronEntry
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = cronEntry
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected
