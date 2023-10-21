module Monadoc.Action.Job.EnqueueSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Job.Enqueue" $ do
  Hspec.it "succeeds" . Test.run $ do
    task <- Test.arbitrary
    model <- Job.Enqueue.run task
    IO.liftIO $ do
      model.key `Hspec.shouldBe` Witch.from @Int 1
      model.value.finishedAt `Hspec.shouldBe` Nothing
      model.value.startedAt `Hspec.shouldBe` Nothing
      model.value.status `Hspec.shouldBe` Status.Queued
      model.value.task `Hspec.shouldBe` task
