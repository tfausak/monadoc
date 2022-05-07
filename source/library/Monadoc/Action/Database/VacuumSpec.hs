module Monadoc.Action.Database.VacuumSpec where

import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Test.Common as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Database.Vacuum" . Hspec.around Test.withConnection $ do
  Hspec.it "runs" . Test.runFake $ do
    Database.Vacuum.run
