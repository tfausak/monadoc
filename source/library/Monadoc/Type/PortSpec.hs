{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.PortSpec where

import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Port as Port
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Port" $ do
  let port = Witch.from @Int @Port.Port 80
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom ("80" :: String) port
