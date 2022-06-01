{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Range.InsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Range.Insert as Range.Insert
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Range.Insert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new range" . Test.runFake $ do
    range <- Test.arbitrary
    actual <- Range.Insert.run range
    let expected =
          Model.Model
            { Model.key = Witch.from @Int 1,
              Model.value = range
            }
    Base.liftBase $ actual `Hspec.shouldBe` expected
