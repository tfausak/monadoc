module Monadoc.Action.CronEntry.UpdateSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Update as CronEntry.Update
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
    result <- App.Sql.query "select * from cronEntry where key = ?" [Model.key cronEntry]
    IO.liftIO $ result `Hspec.shouldBe` [cronEntry]

  Hspec.it "updates a cron entry" . Test.run $ do
    cronEntry1 <- do
      x <- Test.arbitrary
      CronEntry.Insert.run x
    cronEntry2 <- Test.arbitraryWith $ \x -> x {Model.key = Model.key cronEntry1}
    CronEntry.Update.run cronEntry2
    result <- App.Sql.query "select * from cronEntry where key = ?" [Model.key cronEntry2]
    IO.liftIO $ result `Hspec.shouldBe` [cronEntry2]
