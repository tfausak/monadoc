module Monadoc.Query.PackageSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Query.Package as Package.Query
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Package" $ do
  Hspec.describe "getByName" $ do
    Hspec.it "works" . Test.run $ do
      package <- Factory.newPackage
      result <- Package.Query.getByName package.value.name
      IO.liftIO $ result `Hspec.shouldBe` Just package

    Hspec.it "returns nothing when the name doesn't exist" . Test.run $ do
      name <- Test.arbitrary
      result <- Package.Query.getByName name
      IO.liftIO $ result `Hspec.shouldBe` Nothing

  Hspec.describe "getKeys" $ do
    Hspec.it "works with no packages" . Test.run $ do
      result <- Package.Query.getKeys
      IO.liftIO $ result `Hspec.shouldBe` []

    Hspec.it "works with a package" . Test.run $ do
      package <- Factory.newPackage
      result <- Package.Query.getKeys
      IO.liftIO $ result `Hspec.shouldBe` [package.key]
