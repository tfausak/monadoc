module Monadoc.Action.Version.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Version.Upsert as Version.Upsert
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Version.Upsert" $ do
  Hspec.it "inserts a new version" . Test.run $ do
    version <- Test.arbitrary
    model <- Version.Upsert.run version
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = version
          }

  Hspec.it "updates an existing version" . Test.run $ do
    version <- Test.arbitrary
    old <- Version.Upsert.run version
    new <- Version.Upsert.run version
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two versions" . Test.run $ do
    version1 <- do
      x <- Test.arbitraryWith $ \y -> y {Version.number = Witch.unsafeFrom @String "1"}
      Version.Upsert.run x
    version2 <- do
      x <- Test.arbitraryWith $ \y -> y {Version.number = Witch.unsafeFrom @String "2"}
      Version.Upsert.run x
    IO.liftIO $ version1.key `Hspec.shouldNotBe` version2.key
