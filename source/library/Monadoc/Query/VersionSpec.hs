module Monadoc.Query.VersionSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Version as Version.Query
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Version" $ do
  Hspec.describe "getByNumber" $ do
    Hspec.it "works" . Test.run $ do
      version <- Factory.newVersion
      result <- Version.Query.getByNumber . Version.number $ Model.value version
      IO.liftIO $ result `Hspec.shouldBe` Just version

    Hspec.it "returns nothing when the number doesn't exist" . Test.run $ do
      number <- Test.arbitrary
      result <- Version.Query.getByNumber number
      IO.liftIO $ result `Hspec.shouldBe` Nothing
