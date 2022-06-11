module Monadoc.Main.TestSuite where

import qualified MonadocSpec
import qualified Test.Hspec as Hspec

testSuite :: IO ()
testSuite = Hspec.hspec MonadocSpec.spec
