{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.ComponentModule.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Action.ComponentModule.Insert as ComponentModule.Insert
import qualified Monadoc.Action.ComponentModule.Upsert as ComponentModule.Upsert
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Model.ComponentModule as ComponentModule
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.ComponentModule.Upsert" $ do
  Hspec.it "inserts a new component module" . Test.run $ do
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
    actual <- ComponentModule.Insert.run componentModule
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = componentModule
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing component module" . Test.run $ do
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
    old <- ComponentModule.Upsert.run componentModule
    new <- ComponentModule.Upsert.run componentModule
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two component modules" . Test.run $ do
    component <- do
      x <- Test.arbitrary
      Component.Insert.run x
    module1 <- do
      x <- Test.arbitrary
      Module.Insert.run x
    componentModule1 <- Test.arbitraryWith $ \x ->
      x
        { ComponentModule.component = Model.key component,
          ComponentModule.module_ = Model.key module1
        }
    model1 <- ComponentModule.Insert.run componentModule1
    module2 <- do
      x <- Test.arbitrary
      Module.Insert.run x
    componentModule2 <- Test.arbitraryWith $ \x ->
      x
        { ComponentModule.component = Model.key component,
          ComponentModule.module_ = Model.key module2
        }
    model2 <- ComponentModule.Insert.run componentModule2
    IO.liftIO $ Model.key model1 `Hspec.shouldNotBe` Model.key model2
