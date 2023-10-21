module Monadoc.Action.Range.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Range.Upsert" $ do
  Hspec.it "inserts a new range" . Test.run $ do
    range <- Test.arbitrary
    actual <- Range.Upsert.run range
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = range
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing range" . Test.run $ do
    range <- Test.arbitrary
    old <- Range.Upsert.run range
    new <- Range.Upsert.run range
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inesrts two ranges" . Test.run $ do
    range1 <- Test.arbitrary
    range2 <- Test.arbitrary
    model1 <- Range.Upsert.run range1
    model2 <- Range.Upsert.run range2
    IO.liftIO $ model1.key `Hspec.shouldNotBe` model2.key
