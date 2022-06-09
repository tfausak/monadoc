{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.License.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.License.Insert as License.Insert
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.License.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new license" . Test.runFake $ do
    license <- Test.arbitrary
    actual <- License.Insert.run license
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = license
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected
