module Monadoc.Type.FlagSpec where

import qualified Monadoc.Exception.InvalidOption as InvalidOption
import qualified Monadoc.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Monadoc.Exception.UnknownOption as UnknownOption
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Flag as Flag
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Flag" $ do
  Hspec.it "accepts no arguments" $ do
    Flag.fromArguments [] `Hspec.shouldBe` Just []

  Hspec.it "accepts the base flag" $ do
    Flag.fromArguments ["--base-url=x"] `Hspec.shouldBe` Just [Flag.Base "x"]

  Hspec.it "accepts the data flag" $ do
    Flag.fromArguments ["--data-directory=x"] `Hspec.shouldBe` Just [Flag.Data "x"]

  Hspec.it "accepts the dsn flag" $ do
    Flag.fromArguments ["--sentry-dsn=a://b@c/d"] `Hspec.shouldBe` Just [Flag.Dsn "a://b@c/d"]

  Hspec.it "accepts the hackage flag" $ do
    Flag.fromArguments ["--hackage-url=x"] `Hspec.shouldBe` Just [Flag.Hackage "x"]

  Hspec.it "accepts the help flag" $ do
    Flag.fromArguments ["-h"] `Hspec.shouldBe` Just [Flag.Help]
    Flag.fromArguments ["-?"] `Hspec.shouldBe` Just [Flag.Help]
    Flag.fromArguments ["--help"] `Hspec.shouldBe` Just [Flag.Help]

  Hspec.it "accepts the host flag" $ do
    Flag.fromArguments ["--host-preference=x"] `Hspec.shouldBe` Just [Flag.Host "x"]

  Hspec.it "accepts the port flag" $ do
    Flag.fromArguments ["--port-number=x"] `Hspec.shouldBe` Just [Flag.Port "x"]

  Hspec.it "accepts the salt flag" $ do
    Flag.fromArguments ["--proxy-salt=x"] `Hspec.shouldBe` Just [Flag.Salt "x"]

  Hspec.it "accepts the severity flag" $ do
    Flag.fromArguments ["--log-severity=x"] `Hspec.shouldBe` Just [Flag.Severity "x"]

  Hspec.it "accepts the sha flag" $ do
    Flag.fromArguments ["--commit-sha=x"] `Hspec.shouldBe` Just [Flag.Sha "x"]

  Hspec.it "accepts the sql flag" $ do
    Flag.fromArguments ["--database-file=x"] `Hspec.shouldBe` Just [Flag.Sql "x"]

  Hspec.it "accepts the version flag" $ do
    Flag.fromArguments ["--version"] `Hspec.shouldBe` Just [Flag.Version]

  Hspec.it "rejects invalid options" $ do
    Flag.fromArguments ["--help=invalid"] `Hspec.shouldThrow` Test.exceptionSelector @InvalidOption.InvalidOption

  Hspec.it "rejects unknown options" $ do
    Flag.fromArguments ["--unknown"] `Hspec.shouldThrow` Test.exceptionSelector @UnknownOption.UnknownOption

  Hspec.it "rejects unexpected arguments" $ do
    Flag.fromArguments ["unexpected"] `Hspec.shouldThrow` Test.exceptionSelector @UnexpectedArgument.UnexpectedArgument

  Hspec.it "accepts multiple arguments" $ do
    Flag.fromArguments ["--help", "--version"] `Hspec.shouldBe` Just [Flag.Help, Flag.Version]
