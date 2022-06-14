{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.FlagSpec where

import qualified Monadoc.Exception.InvalidOption as InvalidOption
import qualified Monadoc.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Monadoc.Exception.UnknownOption as UnknownOption
import qualified Monadoc.Test as Test
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
    Flag.fromArguments ["--base-url=x"] `Hspec.shouldBe` Just [Flag.Base "x"]
    Flag.fromArguments ["--data-directory=x"] `Hspec.shouldBe` Just [Flag.Data "x"]
    Flag.fromArguments ["--database-file=x"] `Hspec.shouldBe` Just [Flag.Sql "x"]
    Flag.fromArguments ["--hackage-url=x"] `Hspec.shouldBe` Just [Flag.Hackage "x"]
    Flag.fromArguments ["--host-preference=x"] `Hspec.shouldBe` Just [Flag.Host "x"]
    Flag.fromArguments ["--log-severity=x"] `Hspec.shouldBe` Just [Flag.Severity "x"]
    Flag.fromArguments ["--port-number=x"] `Hspec.shouldBe` Just [Flag.Port "x"]
    Flag.fromArguments ["--sentry-dsn=a://b@c/d"] `Hspec.shouldBe` Just [Flag.Dsn "a://b@c/d"]

  Hspec.it "rejects invalid options" $ do
    Flag.fromArguments ["--help=invalid"] `Hspec.shouldThrow` Test.exceptionSelector @InvalidOption.InvalidOption

  Hspec.it "rejects unknown options" $ do
    Flag.fromArguments ["--unknown"] `Hspec.shouldThrow` Test.exceptionSelector @UnknownOption.UnknownOption

  Hspec.it "rejects unexpected arguments" $ do
    Flag.fromArguments ["unexpected"] `Hspec.shouldThrow` Test.exceptionSelector @UnexpectedArgument.UnexpectedArgument
