module Monadoc.Action.Component.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Component.Insert" $ do
  Hspec.it "inserts a new component" . Test.run $ do
    component <- Test.arbitrary
    actual <- Component.Insert.run component
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = component
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected
