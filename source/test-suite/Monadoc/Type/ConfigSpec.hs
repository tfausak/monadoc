module Monadoc.Type.ConfigSpec where

import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Config" $ do
  Hspec.it "can be converted from flags" $ do
    Config.fromFlags [] `Hspec.shouldBe` Just Config.initial
    Config.fromFlags [Flag.Base "x"] `Hspec.shouldBe` Just Config.initial {Config.base = "x/"}
    Config.fromFlags [Flag.Data "x"] `Hspec.shouldBe` Just Config.initial {Config.data_ = Just "x"}
    Config.fromFlags [Flag.Hackage "x"] `Hspec.shouldBe` Just Config.initial {Config.hackage = "x/"}
    Config.fromFlags [Flag.Help] `Hspec.shouldBe` Just Config.initial {Config.help = True}
    Config.fromFlags [Flag.Host "x"] `Hspec.shouldBe` Just Config.initial {Config.host = "x"}
    Config.fromFlags [Flag.Port "80"] `Hspec.shouldBe` Just Config.initial {Config.port = Witch.from @Int 80}
    Config.fromFlags [Flag.Sql "x"] `Hspec.shouldBe` Just Config.initial {Config.sql = "x"}
    Config.fromFlags [Flag.Version] `Hspec.shouldBe` Just Config.initial {Config.version = True}
  Hspec.it "rejects invalid flags" $ do
    Config.fromFlags [Flag.Port "x"] `Hspec.shouldBe` Nothing
