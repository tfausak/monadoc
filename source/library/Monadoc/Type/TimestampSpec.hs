{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.TimestampSpec where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Test as Test
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Timestamp" $ do
  let timestamp = Witch.into @Timestamp.Timestamp $ Time.makeUtcTime 2001 2 3 4 5 6.007
  Hspec.it "can be converted into a string" $ do
    Test.expectFrom timestamp ("2001-02-03T04:05:06.007Z" :: String)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField timestamp $ Sql.SQLText "2001-02-03 04:05:06.007"

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Timestamp.Timestamp)

  Hspec.it "can be rendered as HTML" $ do
    Test.expectHtml timestamp "2001-02-03T04:05:06.007Z"
    Test.expectHtmlRaw timestamp "2001-02-03T04:05:06.007Z"
