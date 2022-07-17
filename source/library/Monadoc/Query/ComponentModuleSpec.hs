module Monadoc.Query.ComponentModuleSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Action.ComponentModule.Insert as ComponentModule.Insert
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Query.ComponentModule as ComponentModule
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.ComponentModule" $ do
  Hspec.describe "selectByComponentAndModule" $ do
    Hspec.it "works when there is no component module" . Test.run $ do
      component <- Test.arbitrary
      module_ <- Test.arbitrary
      result <- ComponentModule.selectByComponentAndModule component module_
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "works when there is a component module" . Test.run $ do
      component <- do
        x <- Test.arbitrary
        Component.Insert.run x
      module_ <- do
        x <- Test.arbitrary
        Module.Insert.run x
      componentModule <- Test.arbitraryWith $ \x ->
        x
          { ComponentModule.component = Model.key component,
            ComponentModule.module_ = Model.key module_
          }
      model <- ComponentModule.Insert.run componentModule
      result <-
        ComponentModule.selectByComponentAndModule
          (Model.key component)
          (Model.key module_)
      IO.liftIO $ result `Hspec.shouldBe` Just model
