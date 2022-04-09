module Monadoc.Action.HackageUser.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.HackageUser.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new hackage user" . Test.runFake $ do
    hackageUser <- Test.arbitrary
    model <- HackageUser.Upsert.run hackageUser
    Base.liftBase $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = hackageUser}

  Hspec.it "updates an existing hackage user" . Test.runFake $ do
    hackageUser <- Test.arbitrary
    old <- HackageUser.Upsert.run hackageUser
    new <- HackageUser.Upsert.run hackageUser
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two hackage users" . Test.runFake $ do
    x <- HackageUser.Upsert.run =<< Test.arbitrary
    y <- HackageUser.Upsert.run =<< Test.arbitrary
    Base.liftBase $ Model.key x `Hspec.shouldNotBe` Model.key y
