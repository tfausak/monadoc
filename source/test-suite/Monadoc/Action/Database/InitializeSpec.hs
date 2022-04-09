module Monadoc.Action.Database.InitializeSpec where

import qualified Monadoc.Action.Database.Initialize as Database.Initialize
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Database.Initialize" . Hspec.around Test.withConnection $ do
  Hspec.it "runs" . Test.runFake $ do
    Database.Initialize.run
