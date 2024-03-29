module Monadoc.Action.Database.VacuumSpec where

import qualified Monadoc.Action.Database.Vacuum as Database.Vacuum
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Database.Vacuum" $ do
  Hspec.it "runs" . Test.run $ do
    Database.Vacuum.run
