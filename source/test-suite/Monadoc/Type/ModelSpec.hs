module Monadoc.Type.ModelSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Model" $ do
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlRow
      Model.Model {Model.key = Key.zero, Model.value = Sql.Only True}
      [Sql.SQLInteger 0, Sql.SQLInteger 1]
