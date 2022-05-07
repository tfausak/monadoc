module Monadoc.Main.TestSuite where

import qualified Monadoc.Test.Spec as Monadoc
import qualified Test.Hspec as Hspec

testSuite :: IO ()
testSuite = Hspec.hspec Monadoc.spec
