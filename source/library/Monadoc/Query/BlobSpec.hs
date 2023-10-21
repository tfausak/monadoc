module Monadoc.Query.BlobSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Factory as Factory
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Query.Blob as Blob.Query
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Blob" $ do
  Hspec.describe "getByHash" $ do
    Hspec.it "works" . Test.run $ do
      blob <- Factory.newBlob
      result <- Blob.Query.getByHash blob.value.hash
      IO.liftIO $ result `Hspec.shouldBe` Just blob

    Hspec.it "returns nothing when the hash doesn't exist" . Test.run $ do
      hash <- Test.arbitrary
      result <- Blob.Query.getByHash hash
      IO.liftIO $ result `Hspec.shouldBe` Nothing
