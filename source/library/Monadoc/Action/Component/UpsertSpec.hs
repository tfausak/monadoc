{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Component.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Component.Upsert as Component.Upsert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Component.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new component" . Test.runFake $ do
    component <- Test.arbitrary
    model <- Component.Upsert.run component
    Base.liftBase $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = component
          }

  Hspec.it "updates an existing component" . Test.runFake $ do
    component <- Test.arbitrary
    old <- Component.Upsert.run component
    new <- Component.Upsert.run component
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two components" . Test.runFake $ do
    component1 <- do
      x <- Test.arbitraryWith $ \y -> y {Component.name = Witch.unsafeFrom @String "a"}
      Component.Upsert.run x
    component2 <- do
      x <- Test.arbitraryWith $ \y -> y {Component.name = Witch.unsafeFrom @String "b"}
      Component.Upsert.run x
    Base.liftBase $ Model.key component1 `Hspec.shouldNotBe` Model.key component2
