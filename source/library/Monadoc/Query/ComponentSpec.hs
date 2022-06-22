module Monadoc.Query.ComponentSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Query.Component as Component
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Component" . Hspec.around Test.withConnection $ do
  Hspec.describe "selectByKeys" $ do
    Hspec.it "works when there is no component" . Test.runFake $ do
      result <- Component.selectByKeys []
      Base.liftBase $ result `Hspec.shouldBe` []

    Hspec.it "works when there is a component" . Test.runFake $ do
      component <- Test.arbitrary
      model <- Component.Insert.run component
      result <- Component.selectByKeys [Model.key model]
      Base.liftBase $ result `Hspec.shouldBe` [model]

  Hspec.describe "selectByTypeAndName" $ do
    Hspec.it "works when there is no component" . Test.runFake $ do
      type_ <- Test.arbitrary
      name <- Test.arbitrary
      result <- Component.selectByTypeAndName type_ name
      Base.liftBase $ result `Hspec.shouldBe` Nothing

    Hspec.it "works when there is a component" . Test.runFake $ do
      component <- Test.arbitrary
      model <- Component.Insert.run component
      result <- Component.selectByTypeAndName (Component.type_ component) (Component.name component)
      Base.liftBase $ result `Hspec.shouldBe` Just model
