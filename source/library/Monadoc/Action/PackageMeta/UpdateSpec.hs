{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.PackageMeta.UpdateSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMeta.Update as PackageMeta.Update
import qualified Monadoc.Model.PackageMeta as PackageMeta
import qualified Monadoc.Query.PackageMeta as PackageMeta
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.PackageMeta.Update" $ do
  Hspec.it "does not throw an error when the package meta doesn't exist" . Test.run $ do
    packageMeta <- Test.arbitrary
    PackageMeta.Update.run packageMeta

  Hspec.it "succeeds when the package meta doesn't need to be updated" . Test.run $ do
    model <- PackageMeta.InsertSpec.insertPackageMeta
    PackageMeta.Update.run model
    result <- PackageMeta.selectByKey $ Model.key model
    Base.liftBase $ result `Hspec.shouldBe` Just model

  Hspec.it "updates a package meta" . Test.run $ do
    old <- PackageMeta.InsertSpec.insertPackageMeta
    let new = old {Model.value = (Model.value old) {PackageMeta.hash = Hash.new "updated"}}
    PackageMeta.Update.run new
    result <- PackageMeta.selectByKey $ Model.key old
    Base.liftBase $ result `Hspec.shouldBe` Just new
