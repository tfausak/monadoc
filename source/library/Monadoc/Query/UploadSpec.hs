module Monadoc.Query.UploadSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Query.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Upload" $ do
  Hspec.describe "selectByPackageAndVersionAndRevision" $ do
    Hspec.it "returns nothing when there is no upload" . Test.run $ do
      package <- Test.arbitrary
      version <- Test.arbitrary
      revision <- Test.arbitrary
      result <- Upload.selectByPackageAndVersionAndRevision package version revision
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a upload when one exists" . Test.run $ do
      blob <- do
        x <- Test.arbitrary
        Blob.Upsert.run x
      hackageUser <- do
        x <- Test.arbitrary
        HackageUser.Upsert.run x
      package <- do
        x <- Test.arbitrary
        Package.Upsert.run x
      version <- do
        x <- Test.arbitrary
        Version.Upsert.run x
      upload <- Test.arbitraryWith $ \x ->
        x
          { Upload.blob = Model.key blob,
            Upload.package = Model.key package,
            Upload.uploadedBy = Model.key hackageUser,
            Upload.version = Model.key version
          }
      model <- Upload.Upsert.run upload
      result <- Upload.selectByPackageAndVersionAndRevision (Model.key package) (Model.key version) (Upload.revision upload)
      IO.liftIO $ result `Hspec.shouldBe` Just model
