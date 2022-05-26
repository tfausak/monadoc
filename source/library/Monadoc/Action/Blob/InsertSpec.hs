{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Blob.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Blob.Insert as Blob.Insert
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Blob.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new blob" . Test.runFake $ do
    let blob = Blob.new ""
    actual <- Blob.Insert.run blob
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = blob
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected
