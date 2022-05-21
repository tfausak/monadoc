{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Package.UpsertSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Package.Upsert" . Hspec.around Test.withConnection $ do
  Hspec.it "inserts a new package" . Test.runFake $ do
    package <- Test.arbitrary
    model <- Package.Upsert.run package
    Base.liftBase $ model `Hspec.shouldBe` Model.Model {Model.key = Witch.from @Int 1, Model.value = package}

  Hspec.it "updates an existing package" . Test.runFake $ do
    package <- Test.arbitrary
    old <- Package.Upsert.run package
    new <- Package.Upsert.run package
    Base.liftBase $ new `Hspec.shouldBe` old

  Hspec.it "inserts two packages" . Test.runFake $ do
    a <- Package.Upsert.run =<< Test.arbitraryWith (\x -> x {Package.name = Witch.unsafeFrom @String "a"})
    b <- Package.Upsert.run =<< Test.arbitraryWith (\x -> x {Package.name = Witch.unsafeFrom @String "b"})
    Base.liftBase $ Model.key a `Hspec.shouldNotBe` Model.key b