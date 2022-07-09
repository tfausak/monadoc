{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.CronEntry.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Insert" $ do
  Hspec.it "inserts a new cron entry" . Test.run $ do
    cronEntry <- Test.arbitrary
    actual <- CronEntry.Insert.run cronEntry
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = cronEntry
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected
