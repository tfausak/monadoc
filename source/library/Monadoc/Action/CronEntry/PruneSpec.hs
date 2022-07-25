module Monadoc.Action.CronEntry.PruneSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.CronEntry.Insert as CronEntry.Insert
import qualified Monadoc.Action.CronEntry.Prune as CronEntry.Prune
import qualified Monadoc.Constant.CronEntry as CronEntry
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.CronEntry.Prune" $ do
  Hspec.it "works when there are no cron entries" . Test.run $ do
    CronEntry.Prune.run

  Hspec.it "keeps a cron entry with no guid" . Test.run $ do
    cronEntry <- do
      x <- Test.arbitraryWith $ \y -> y {CronEntry.guid = Nothing}
      CronEntry.Insert.run x
    CronEntry.Prune.run
    result <- App.Sql.query "select * from cronEntry where key = ?" [Model.key cronEntry]
    IO.liftIO $ result `Hspec.shouldBe` [cronEntry]

  Hspec.it "removes a cron entry with a guid" . Test.run $ do
    guid <- Test.arbitrary
    cronEntry <- do
      x <- Test.arbitraryWith $ \y -> y {CronEntry.guid = Just guid}
      CronEntry.Insert.run x
    CronEntry.Prune.run
    result <- App.Sql.query @CronEntry.Model "select * from cronEntry where key = ?" [Model.key cronEntry]
    IO.liftIO $ result `Hspec.shouldBe` []

  Hspec.it "keeps a cron entry with a static guid" . Test.run $
    case fmap CronEntry.guid CronEntry.all of
      Just guid : _ -> do
        cronEntry <- do
          x <- Test.arbitraryWith $ \y -> y {CronEntry.guid = Just guid}
          CronEntry.Insert.run x
        CronEntry.Prune.run
        result <- App.Sql.query "select * from cronEntry where key = ?" [Model.key cronEntry]
        IO.liftIO $ result `Hspec.shouldBe` [cronEntry]
      _ -> IO.liftIO $ False `Hspec.shouldBe` True
