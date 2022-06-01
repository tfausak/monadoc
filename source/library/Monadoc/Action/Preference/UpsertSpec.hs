{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Preference.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Action.Preference.Upsert as Preference.Upsert
import qualified Monadoc.Action.Range.Upsert as Range.Upsert
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Constraint as Constraint
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.PackageName as PackageName
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Preference.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new preference" . Test.runFake $ do
    package <- do
      x <- Test.arbitrary
      Package.Upsert.run x
    range <- do
      x <- Test.arbitrary
      Range.Upsert.run x
    let preference =
          Preference.Preference
            { Preference.package = Model.key package,
              Preference.range = Model.key range
            }
    model <- Preference.Upsert.run preference
    Base.liftBase $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = preference
          }

  Hspec.it "updates an existing preference" . Test.runFake $ do
    old <- upsertPreference (Witch.unsafeFrom @String "a") (Witch.unsafeFrom @String ">1")
    new <- upsertPreference (Witch.unsafeFrom @String "a") (Witch.unsafeFrom @String ">2")
    Base.liftBase $ do
      Model.key new `Hspec.shouldBe` Model.key old
      Preference.range (Model.value new) `Hspec.shouldNotBe` Preference.range (Model.value old)

  Hspec.it "inserts two preferences" . Test.runFake $ do
    preference1 <- upsertPreference (Witch.unsafeFrom @String "a") (Witch.unsafeFrom @String ">1")
    preference2 <- upsertPreference (Witch.unsafeFrom @String "b") (Witch.unsafeFrom @String ">1")
    Base.liftBase $ Model.key preference1 `Hspec.shouldNotBe` Model.key preference2

upsertPreference ::
  (MonadSql.MonadSql m, Exception.MonadThrow m) =>
  PackageName.PackageName ->
  Constraint.Constraint ->
  m Preference.Model
upsertPreference packageName constraint = do
  package <- Package.Upsert.run Package.Package {Package.name = packageName}
  range <- Range.Upsert.run Range.Range {Range.constraint = constraint}
  Preference.Upsert.run
    Preference.Preference
      { Preference.package = Model.key package,
        Preference.range = Model.key range
      }
