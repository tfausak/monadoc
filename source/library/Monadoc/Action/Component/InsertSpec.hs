{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Component.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Component.Insert as Component.Insert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Component.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new component" . Test.runFake $ do
    component <- Test.arbitrary
    actual <- Component.Insert.run component
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = component
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected