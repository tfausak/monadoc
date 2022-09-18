module Monadoc.Type.ConfigSpec where

import qualified Monadoc.Exception.InvalidDsn as InvalidDsn
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Flag as Flag
import qualified Monadoc.Type.Port as Port
import qualified Monadoc.Type.Severity as Severity
import qualified Network.URI as Uri
import qualified Patrol.Type.Dsn as Patrol.Dsn
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Config" $ do
  Hspec.it "returns the initial config when given no flags" $ do
    Config.fromFlags [] `Hspec.shouldBe` Just Config.initial

  Hspec.it "accepts the base flag" $ do
    Config.fromFlags [Flag.Base "x"] `Hspec.shouldBe` Just Config.initial {Config.base = "x/"}

  Hspec.it "accepts the data flag" $ do
    Config.fromFlags [Flag.Data "x"] `Hspec.shouldBe` Just Config.initial {Config.data_ = "x"}

  Hspec.it "accepts the dsn flag" $ do
    uri <- maybe (fail "invalid DSN") pure $ Uri.parseURI "a://b@c/d"
    dsn <- Patrol.Dsn.fromUri uri
    Config.fromFlags [Flag.Dsn "a://b@c/d"] `Hspec.shouldBe` Just Config.initial {Config.dsn = Just dsn}

  Hspec.it "rejects an invalid DSN" $ do
    Config.fromFlags [Flag.Dsn "x"] `Hspec.shouldThrow` Test.exceptionSelector @InvalidDsn.InvalidDsn

  Hspec.it "accepts the hackage flag" $ do
    Config.fromFlags [Flag.Hackage "x"] `Hspec.shouldBe` Just Config.initial {Config.hackage = "x/"}

  Hspec.it "accepts the help flag" $ do
    Config.fromFlags [Flag.Help] `Hspec.shouldBe` Just Config.initial {Config.help = True}

  Hspec.it "accepts the host flag" $ do
    Config.fromFlags [Flag.Host "x"] `Hspec.shouldBe` Just Config.initial {Config.host = "x"}

  Hspec.it "accepts the port flag" $ do
    Config.fromFlags [Flag.Port "80"] `Hspec.shouldBe` Just Config.initial {Config.port = Witch.from @Int 80}

  Hspec.it "rejects an invalid port" $ do
    Config.fromFlags [Flag.Port "x"] `Hspec.shouldThrow` Test.exceptionSelector @(Witch.TryFromException String Port.Port)

  Hspec.it "accepts the salt flag" $ do
    Config.fromFlags [Flag.Salt "x"] `Hspec.shouldBe` Just Config.initial {Config.salt = "x"}

  Hspec.it "accepts the severity flag" $ do
    Config.fromFlags [Flag.Severity "info"] `Hspec.shouldBe` Just Config.initial {Config.severity = Severity.Info}

  Hspec.it "rejects an invalid severity" $ do
    Config.fromFlags [Flag.Severity "x"] `Hspec.shouldThrow` Test.exceptionSelector @(Witch.TryFromException String Severity.Severity)

  Hspec.it "accepts the sha flag" $ do
    Config.fromFlags [Flag.Sha "x"] `Hspec.shouldBe` Just Config.initial {Config.sha = "x"}

  Hspec.it "accepts the sql flag" $ do
    Config.fromFlags [Flag.Sql "x"] `Hspec.shouldBe` Just Config.initial {Config.sql = "x"}

  Hspec.it "accepts the version flag" $ do
    Config.fromFlags [Flag.Version] `Hspec.shouldBe` Just Config.initial {Config.version = True}

  Hspec.it "accepts multiple flags" $ do
    Config.fromFlags [Flag.Help, Flag.Version] `Hspec.shouldBe` Just Config.initial {Config.help = True, Config.version = True}

  Hspec.it "overrides earlier flags with later ones" $ do
    Config.fromFlags [Flag.Sha "x", Flag.Sha "y"] `Hspec.shouldBe` Just Config.initial {Config.sha = "y"}
