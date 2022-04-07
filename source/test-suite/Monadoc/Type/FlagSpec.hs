module Monadoc.Type.FlagSpec where

import qualified Monadoc.Type.Flag as Flag
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Flag" $ do
  Hspec.it "parses command line arguments" $ do
    Flag.fromArguments [] `Hspec.shouldBe` Just []
    Flag.fromArguments ["-h"] `Hspec.shouldBe` Just [Flag.Help]
    Flag.fromArguments ["-?"] `Hspec.shouldBe` Just [Flag.Help]
    Flag.fromArguments ["--help"] `Hspec.shouldBe` Just [Flag.Help]
    Flag.fromArguments ["-v"] `Hspec.shouldBe` Just [Flag.Version]
    Flag.fromArguments ["--version"] `Hspec.shouldBe` Just [Flag.Version]
    Flag.fromArguments ["--base=x"] `Hspec.shouldBe` Just [Flag.Base "x"]
    Flag.fromArguments ["--data=x"] `Hspec.shouldBe` Just [Flag.Data "x"]
    Flag.fromArguments ["--hackage=x"] `Hspec.shouldBe` Just [Flag.Hackage "x"]
    Flag.fromArguments ["--host=x"] `Hspec.shouldBe` Just [Flag.Host "x"]
    Flag.fromArguments ["--port=x"] `Hspec.shouldBe` Just [Flag.Port "x"]
    Flag.fromArguments ["--sql=x"] `Hspec.shouldBe` Just [Flag.Sql "x"]
  Hspec.it "rejects invalid options" $ do
    Flag.fromArguments ["--help=invalid"] `Hspec.shouldBe` Nothing
  Hspec.it "rejects unknown options" $ do
    Flag.fromArguments ["--unknown"] `Hspec.shouldBe` Nothing
  Hspec.it "rejects unexpected arguments" $ do
    Flag.fromArguments ["unexpected"] `Hspec.shouldBe` Nothing
