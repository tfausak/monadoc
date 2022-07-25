module Monadoc.Action.HackageUser.UpsertSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Text as Text
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.HackageUser.Upsert" $ do
  Hspec.it "inserts a new hackage user" . Test.run $ do
    hackageUser <- Test.arbitrary
    model <- HackageUser.Upsert.run hackageUser
    IO.liftIO $
      model
        `Hspec.shouldBe` Model.Model
          { Model.key = Witch.from @Int 1,
            Model.value = hackageUser
          }

  Hspec.it "updates an existing hackage user" . Test.run $ do
    hackageUser <- Test.arbitrary
    old <- HackageUser.Upsert.run hackageUser
    new <- HackageUser.Upsert.run hackageUser
    IO.liftIO $ new `Hspec.shouldBe` old

  Hspec.it "inserts two hackage users" . Test.run $ do
    hackageUser1 <- do
      x <- Test.arbitraryWith $ \y -> y {HackageUser.name = Witch.unsafeFrom @Text.Text "a"}
      HackageUser.Upsert.run x
    hackageUser2 <- do
      x <- Test.arbitraryWith $ \y -> y {HackageUser.name = Witch.unsafeFrom @Text.Text "b"}
      HackageUser.Upsert.run x
    IO.liftIO $ Model.key hackageUser1 `Hspec.shouldNotBe` Model.key hackageUser2
