module Monadoc.Query.PackageSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Query.Package as Package
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Package" $ do
  Hspec.describe "selectByName" $ do
    Hspec.it "returns nothing when there is no package" . Test.run $ do
      name <- Test.arbitrary
      result <- Package.selectByName name
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a package when one exists" . Test.run $ do
      package <- Test.arbitrary
      model <- Package.Upsert.run package
      result <- Package.selectByName . Package.name $ Model.value model
      IO.liftIO $ result `Hspec.shouldBe` Just model
