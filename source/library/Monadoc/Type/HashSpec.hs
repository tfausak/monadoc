{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.HashSpec where

import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Hash as Hash
import qualified Test.Hspec as Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Hash" $ do
  let hash = Hash.new mempty
      byteString :: ByteString.ByteString
      byteString = "\xe3\xb0\xc4\x42\x98\xfc\x1c\x14\x9a\xfb\xf4\xc8\x99\x6f\xb9\x24\x27\xae\x41\xe4\x64\x9b\x93\x4c\xa4\x95\x99\x1b\x78\x52\xb8\x55"

  Hspec.it "can be converted from a byte string" $ do
    Test.expectTryFrom byteString hash

  Hspec.it "can be converted into a byte string" $ do
    Test.expectFrom hash byteString

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField hash $ Sql.SQLBlob byteString

  Hspec.it "can be round-tripped through SQL" $
    QuickCheck.property (Test.propertySqlField @Hash.Hash)
