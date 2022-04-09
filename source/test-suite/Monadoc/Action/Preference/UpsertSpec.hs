module Monadoc.Action.Preference.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Preference.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new preference" . Test.runFake $ do
    package <- Package.Upsert.run =<< Test.arbitrary
    preference <- Test.arbitraryWith $ \preference -> preference {Preference.package = Model.key package}
    model <- Preference.Upsert.run preference
    Base.liftBase $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = preference}

  Hspec.it "updates an existing preference" . Test.runFake $ do
    package <- Package.Upsert.run =<< Test.arbitrary
    preference <- Test.arbitraryWith $ \preference -> preference {Preference.package = Model.key package}
    old <- Preference.Upsert.run preference
    constraint <- Test.arbitrary
    new <- Preference.Upsert.run preference {Preference.constraint = constraint}
    Base.liftBase $ do
      Model.key new `Hspec.shouldBe` Model.key old
      Preference.constraint (Model.value new) `Hspec.shouldBe` constraint

  Hspec.it "inserts two preferences" . Test.runFake $ do
    x <- do
      package <- Package.Upsert.run =<< Test.arbitrary
      preference <- Test.arbitraryWith $ \preference -> preference {Preference.package = Model.key package}
      Preference.Upsert.run preference
    y <- do
      package <- Package.Upsert.run =<< Test.arbitrary
      preference <- Test.arbitraryWith $ \preference -> preference {Preference.package = Model.key package}
      Preference.Upsert.run preference
    Base.liftBase $ Model.key x `Hspec.shouldNotBe` Model.key y