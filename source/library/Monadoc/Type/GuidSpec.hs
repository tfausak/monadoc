{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.GuidSpec where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Test.Common as Test
import qualified Monadoc.Type.Guid as Guid
import qualified Test.Hspec as Hspec
import qualified Witch

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Type.Guid" $ do
  Hspec.it "can be converted from a byte string" $ do
    Test.expectTryFrom (LazyByteString.replicate 16 0x00) (Witch.into @Guid.Guid Uuid.nil)

  Hspec.it "can be converted into a byte string" $ do
    Test.expectFrom (Witch.into @Guid.Guid Uuid.nil) (LazyByteString.replicate 16 0x00)

  Hspec.it "can be round-tripped through SQL" $ do
    Test.expectSqlField (Witch.into @Guid.Guid Uuid.nil) (Sql.SQLBlob "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00")
