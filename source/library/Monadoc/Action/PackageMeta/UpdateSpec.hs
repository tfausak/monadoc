module Monadoc.Action.PackageMeta.UpdateSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Action.PackageMeta.InsertSpec as PackageMeta.InsertSpec
import qualified Monadoc.Action.PackageMeta.Update as PackageMeta.Update
import qualified Monadoc.Model.PackageMeta as PackageMeta
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
    result <- App.Sql.query "select * from packageMeta where key = ? limit 1" [Model.key model]
    IO.liftIO $ result `Hspec.shouldBe` [model]

  Hspec.it "updates a package meta" . Test.run $ do
    old <- PackageMeta.InsertSpec.insertPackageMeta
    let new = old {Model.value = (Model.value old) {PackageMeta.hash = Hash.new "updated"}}
    PackageMeta.Update.run new
    result <- App.Sql.query "select * from packageMeta where key = ? limit 1" [Model.key old]
    IO.liftIO $ result `Hspec.shouldBe` [new]
