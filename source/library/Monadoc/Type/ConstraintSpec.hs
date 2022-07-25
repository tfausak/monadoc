module Monadoc.Type.ConstraintSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Constraint as Constraint
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Constraint" $ do
  Hspec.it "can be converted from a string" $ do
    Test.expectTryFrom (">=0" :: String) Constraint.any

  Hspec.it "can be converted into a string" $ do
    Test.expectFrom Constraint.any (">=0" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField Constraint.any $ Sql.SQLText ">=0"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Constraint.Constraint)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml Constraint.any "&gt;=0"
    Test.expectHtmlRaw Constraint.any ">=0"
