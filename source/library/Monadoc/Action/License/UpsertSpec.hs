{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.License.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.License.Upsert as License.Upsert
import qualified Monadoc.Model.License as License
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.License.Upsert" $ do
  Hspec.it "inserts a new license" . Test.run $ do
    license <- Test.arbitrary
    actual <- License.Upsert.run license
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = license
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected

  Hspec.it "updates an existing license" . Test.run $ do
    license <- Test.arbitrary
    old <- License.Upsert.run license
    new <- License.Upsert.run license
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inesrts two licenses" . Test.run $ do
    license1 <- Test.arbitraryWith $ \x -> x {License.spdx = Witch.unsafeFrom @String "MIT"}
    license2 <- Test.arbitraryWith $ \x -> x {License.spdx = Witch.unsafeFrom @String "ISC"}
    model1 <- License.Upsert.run license1
    model2 <- License.Upsert.run license2
    Base.liftBase $ Model.key model1 `Hspec.shouldNotBe` Model.key model2
