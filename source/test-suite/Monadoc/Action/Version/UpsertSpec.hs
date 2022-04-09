module Monadoc.Action.Version.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Distribution.Types.Version as Cabal
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.VersionNumber as VersionNumber
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Version.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new version" . Test.runFake $ do
    let version = Version.Version {Version.number = VersionNumber.zero}
    model <- Version.Upsert.run version
    Base.liftBase $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = version}

  Hspec.it "updates an existing version" . Test.runFake $ do
    let version = Version.Version {Version.number = VersionNumber.zero}
    old <- Version.Upsert.run version
    new <- Version.Upsert.run version
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two versions" . Test.runFake $ do
    a <- Version.Upsert.run Version.Version {Version.number = Witch.from $ Cabal.mkVersion [1]}
    b <- Version.Upsert.run Version.Version {Version.number = Witch.from $ Cabal.mkVersion [2]}
    Base.liftBase $ Model.key a `Hspec.shouldNotBe` Model.key b
