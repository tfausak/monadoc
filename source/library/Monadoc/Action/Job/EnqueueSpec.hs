{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.Job.EnqueueSpec where

import qualified Control.Monad.Base as Base
import qualified Monadoc.Action.Job.Enqueue as Job.Enqueue
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Status as Status
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Action.Job.Enqueue" . Hspec.around Test.withConnection $ do
  Hspec.it "succeeds" . Test.runFake $ do
    task <- Test.arbitrary
    model <- Job.Enqueue.run task
    Base.liftBase $ do
      Model.key model `Hspec.shouldBe` Witch.from @Int 1
      Job.finishedAt (Model.value model) `Hspec.shouldBe` Nothing
      Job.startedAt (Model.value model) `Hspec.shouldBe` Nothing
      Job.status (Model.value model) `Hspec.shouldBe` Status.Queued
      Job.task (Model.value model) `Hspec.shouldBe` task
