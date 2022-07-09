module Monadoc.Query.ComponentSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Model.Component as Component
import qualified Monadoc.Query.Component as Component
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Component" $ do
  Hspec.describe "selectByKeys" $ do
    Hspec.it "works when there is no component" . Test.run $ do
      result <- Component.selectByKeys []
      IO.liftIO $ result `Hspec.shouldBe` []

    Hspec.it "works when there is a component" . Test.run $ do
      component <- Test.arbitrary
      model <- Component.Insert.run component
      result <- Component.selectByKeys [Model.key model]
      IO.liftIO $ result `Hspec.shouldBe` [model]

  Hspec.describe "selectByTypeAndName" $ do
    Hspec.it "works when there is no component" . Test.run $ do
      type_ <- Test.arbitrary
      name <- Test.arbitrary
      result <- Component.selectByTypeAndName type_ name
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "works when there is a component" . Test.run $ do
      component <- Test.arbitrary
      model <- Component.Insert.run component
      result <- Component.selectByTypeAndName (Component.type_ component) (Component.name component)
      IO.liftIO $ result `Hspec.shouldBe` Just model
