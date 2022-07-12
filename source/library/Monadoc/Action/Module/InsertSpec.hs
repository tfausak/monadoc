{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Module.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Module.Insert" $ do
  Hspec.it "inserts a new module" . Test.run $ do
    module_ <- Test.arbitrary
    actual <- Module.Insert.run module_
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = module_
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected
