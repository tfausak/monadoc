module Monadoc.Query.HackageUserSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.HackageUser.Upsert as HackageUser.Upsert
import qualified Monadoc.Query.HackageUser as HackageUser
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.HackageUser" $ do
  Hspec.describe "selectByKey" $ do
    Hspec.it "returns nothing when there is no hackage user" . Test.run $ do
      key <- Test.arbitrary
      result <- HackageUser.selectByKey key
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "returns a hackage user when one exists" . Test.run $ do
      hackageUser <- Test.arbitrary
      model <- HackageUser.Upsert.run hackageUser
      result <- HackageUser.selectByKey $ Model.key model
      IO.liftIO $ result `Hspec.shouldBe` Just model
