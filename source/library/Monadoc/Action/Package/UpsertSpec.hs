module Monadoc.Action.Package.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Package.Upsert as Package.Upsert
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Package.Upsert" $ do
  Hspec.it "inserts a new package" . Test.run $ do
    package <- Test.arbitrary
    model <- Package.Upsert.run package
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = package
          }

  Hspec.it "updates an existing package" . Test.run $ do
    package <- Test.arbitrary
    old <- Package.Upsert.run package
    new <- Package.Upsert.run package
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two packages" . Test.run $ do
    package1 <- do
      x <- Test.arbitraryWith $ \y -> y {Package.name = Witch.unsafeFrom @String "a"}
      Package.Upsert.run x
    package2 <- do
      x <- Test.arbitraryWith $ \y -> y {Package.name = Witch.unsafeFrom @String "b"}
      Package.Upsert.run x
    IO.liftIO $ package1.key `Hspec.shouldNotBe` package2.key
