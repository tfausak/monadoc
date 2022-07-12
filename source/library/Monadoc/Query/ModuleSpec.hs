module Monadoc.Query.ModuleSpec where

import qualified Control.Monad.IO.Class as IO
import qualified Monadoc.Action.Module.Insert as Module.Insert
import qualified Monadoc.Model.Module as Module
import qualified Monadoc.Query.Module as Module
import qualified Monadoc.Test as Test
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Query.Module" $ do
  Hspec.describe "selectByName" $ do
    Hspec.it "works when there is no module" . Test.run $ do
      moduleName <- Test.arbitrary
      result <- Module.selectByName moduleName
      IO.liftIO $ result `Hspec.shouldBe` Nothing

    Hspec.it "works when there is a module" . Test.run $ do
      module_ <- Test.arbitrary
      model <- Module.Insert.run module_
      result <- Module.selectByName $ Module.name module_
      IO.liftIO $ result `Hspec.shouldBe` Just model
