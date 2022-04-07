module Monadoc.Type.HackageUserNameSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.HackageUserName" $ do
  let hackageUserName = Witch.from @String @HackageUserName.HackageUserName "example"
  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField hackageUserName $ Sql.SQLText "example"
  Hspec.it "can be rendered to HTML" $ do
    Test.expectHtml hackageUserName "example"
    Test.expectHtmlRaw hackageUserName "example"
