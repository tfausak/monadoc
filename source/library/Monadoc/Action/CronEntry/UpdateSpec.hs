module Monadoc.Action.CronEntry.UpdateSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
import qualified Monadoc.Query.CronEntry as CronEntry.Query
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Update" $ do
  Hspec.it "does not throw an error when the cron entry doesn't exist" . Test.run $ do
    cronEntry <- Test.arbitrary
    CronEntry.Update.run cronEntry

  Hspec.it "succeeds when the cron entry doesn't need to be updated" . Test.run $ do
    cronEntry <- do
      x <- Test.arbitrary
      CronEntry.Insert.run x
    CronEntry.Update.run cronEntry
    result <- CronEntry.Query.getByKey cronEntry.key
    IO.liftIO $ result `Hspec.shouldBe` Just cronEntry

  Hspec.it "updates a cron entry" . Test.run $ do
    cronEntry1 <- do
      x <- Test.arbitrary
      CronEntry.Insert.run x
    cronEntry2 <- Test.arbitraryWith $ \x -> x {Model.key = cronEntry1.key}
    CronEntry.Update.run cronEntry2
    result <- CronEntry.Query.getByKey cronEntry2.key
    IO.liftIO $ result `Hspec.shouldBe` Just cronEntry2
