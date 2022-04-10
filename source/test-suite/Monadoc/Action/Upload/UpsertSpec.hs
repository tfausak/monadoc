module Monadoc.Action.Upload.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Upload.Upsert as Upload.Upsert
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Upload.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new upload" . Test.runFake $ do
    blob <- Blob.Upsert.run =<< Test.arbitrary
    hackageUser <- HackageUser.Upsert.run =<< Test.arbitrary
    package <- Package.Upsert.run =<< Test.arbitrary
    version <- Version.Upsert.run =<< Test.arbitrary
    upload <- Test.arbitraryWith $ \x ->
      x
        { Upload.blob = Model.key blob,
          Upload.package = Model.key package,
          Upload.uploadedBy = Model.key hackageUser,
          Upload.version = Model.key version
        }
    model <- Upload.Upsert.run upload
    Base.liftBase $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = upload}

  Hspec.it "updates an existing upload" . Test.runFake $ do
    blob <- Blob.Upsert.run =<< Test.arbitrary
    hackageUser <- HackageUser.Upsert.run =<< Test.arbitrary
    package <- Package.Upsert.run =<< Test.arbitrary
    version <- Version.Upsert.run =<< Test.arbitrary
    upload <- Test.arbitraryWith $ \x ->
      x
        { Upload.blob = Model.key blob,
          Upload.package = Model.key package,
          Upload.uploadedBy = Model.key hackageUser,
          Upload.version = Model.key version
        }
    old <- Upload.Upsert.run upload
    new <- Upload.Upsert.run upload
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two uploads" . Test.runFake $ do
    blob <- Blob.Upsert.run =<< Test.arbitrary
    hackageUser <- HackageUser.Upsert.run =<< Test.arbitrary
    package <- Package.Upsert.run =<< Test.arbitrary
    version <- Version.Upsert.run =<< Test.arbitrary
    a <-
      Upload.Upsert.run
        =<< Test.arbitraryWith
          ( \x ->
              x
                { Upload.blob = Model.key blob,
                  Upload.package = Model.key package,
                  Upload.revision = Witch.from @Word 1,
                  Upload.uploadedBy = Model.key hackageUser,
                  Upload.version = Model.key version
                }
          )
    b <-
      Upload.Upsert.run
        =<< Test.arbitraryWith
          ( \x ->
              x
                { Upload.blob = Model.key blob,
                  Upload.package = Model.key package,
                  Upload.revision = Witch.from @Word 2,
                  Upload.uploadedBy = Model.key hackageUser,
                  Upload.version = Model.key version
                }
          )
    Base.liftBase $ Model.key a `Hspec.shouldNotBe` Model.key b
