module Monadoc.Query.VersionSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Query.Version as Version
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Version" $ do
  Hspec.describe "selectByNumber" $ do
    Hspec.it "returns nothing when there is no version" . Test.run $ do
      number <- Test.arbitrary
      result <- Version.selectByNumber number
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a version when one exists" . Test.run $ do
      version <- Test.arbitrary
      model <- Version.Upsert.run version
      result <- Version.selectByNumber . Version.number $ Model.value model
      IO.liftIO $ result `Hspec.shouldBe` Just model
