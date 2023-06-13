module Monadoc.Query.CronEntrySpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry.Query
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.CronEntry" $ do
  Hspec.describe "getByGuid" $ do
    Hspec.it "works" . Test.run $ do
      guid <- Test.arbitrary
      cronEntry <- Test.arbitraryWith $ \x -> x {CronEntry.guid = Just guid}
      model <- CronEntry.Upsert.run cronEntry
      result <- CronEntry.Query.getByGuid guid
      IO.liftIO $ result `Hspec.shouldBe` Just model

    Hspec.it "returns nothing when the guid doesn't exist" . Test.run $ do
      guid <- Test.arbitrary
      result <- CronEntry.Query.getByGuid guid
      IO.liftIO $ result `Hspec.shouldBe` Nothing
