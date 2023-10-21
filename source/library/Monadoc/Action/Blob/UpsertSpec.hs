module Monadoc.Action.Blob.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Blob.Upsert as Blob.Upsert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Blob.Upsert" $ do
  Hspec.it "inserts a new blob" . Test.run $ do
    blob <- Test.arbitrary
    model <- Blob.Upsert.run blob
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = blob
          }

  Hspec.it "updates an existing blob" . Test.run $ do
    blob <- Test.arbitrary
    old <- Blob.Upsert.run blob
    new <- Blob.Upsert.run blob
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two blobs" . Test.run $ do
    blob1 <- Blob.Upsert.run $ Blob.new "a"
    blob2 <- Blob.Upsert.run $ Blob.new "b"
    IO.liftIO $ blob1.key `Hspec.shouldNotBe` blob2.key
