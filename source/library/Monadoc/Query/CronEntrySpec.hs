module Monadoc.Query.CronEntrySpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.CronEntry.Upsert as CronEntry.Upsert
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Query.CronEntry as CronEntry.Query
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
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

  Hspec.describe "getByKey" $ do
    Hspec.it "works" . Test.run $ do
      cronEntry <- Factory.newCronEntry
      result <- CronEntry.Query.getByKey cronEntry.key
      IO.liftIO $ result `Hspec.shouldBe` Just cronEntry

    Hspec.it "returns nothing when the key doesn't exist" . Test.run $ do
      key <- Test.arbitrary
      result <- CronEntry.Query.getByKey key
      IO.liftIO $ result `Hspec.shouldBe` Nothing
