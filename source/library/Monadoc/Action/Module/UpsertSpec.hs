{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Module.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Module.Upsert as Module.Upsert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Module.Upsert" $ do
  Hspec.it "inserts a new module" . Test.run $ do
    module_ <- Test.arbitrary
    model <- Module.Upsert.run module_
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = module_
          }

  Hspec.it "updates an existing module" . Test.run $ do
    module_ <- Test.arbitrary
    old <- Module.Upsert.run module_
    new <- Module.Upsert.run module_
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two modules" . Test.run $ do
    module1 <- Test.arbitrary
    model1 <- Module.Upsert.run module1
    module2 <- Test.arbitrary
    model2 <- Module.Upsert.run module2
    IO.liftIO $ Model.key model1 `Hspec.shouldNotBe` Model.key model2
