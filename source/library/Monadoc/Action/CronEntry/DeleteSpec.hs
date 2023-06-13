module Monadoc.Action.CronEntry.DeleteSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.CronEntry.Delete as CronEntry.Delete
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry.Query
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Delete" $ do
  Hspec.it "does not throw an error when the cron entry doesn't exist" . Test.run $ do
    guid <- Test.arbitrary
    CronEntry.Delete.run guid

  Hspec.it "deletes the cron entry" . Test.run $ do
    guid <- Test.arbitrary
    cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
    Monad.void $ CronEntry.Insert.run cronEntry
    CronEntry.Delete.run guid
    maybeCronEntry <- CronEntry.Query.getByGuid guid
    IO.liftIO $ maybeCronEntry `Hspec.shouldBe` Nothing
