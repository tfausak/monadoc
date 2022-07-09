{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Blob.InsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Blob.Insert" $ do
  Hspec.it "inserts a new blob" . Test.run $ do
    blob <- Test.arbitrary
    actual <- Blob.Insert.run blob
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = blob
            }
    IO.liftIO $ actual `Hspec.shouldBe` expected
