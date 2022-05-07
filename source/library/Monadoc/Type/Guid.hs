{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Guid where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.UUID as Uuid
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Guid
  = Guid Uuid.UUID
  deriving (Eq, Ord, Show)

instance Witch.From Uuid.UUID Guid

instance Witch.From Guid Uuid.UUID

instance Witch.TryFrom LazyByteString.ByteString Guid where
  tryFrom = Witch.maybeTryFrom $ fmap Witch.from . Uuid.fromByteString

instance Witch.From Guid LazyByteString.ByteString where
  from = Uuid.toByteString . Witch.from

instance Sql.FromField Guid where
  fromField field = do
    byteString <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom @LazyByteString.ByteString byteString

instance Sql.ToField Guid where
  toField = Sql.toField . Witch.into @LazyByteString.ByteString

instance QuickCheck.Arbitrary Guid where
  arbitrary = Witch.from <$> genUuid

instance Witch.From Guid Text.Text where
  from = Uuid.toText . Witch.from

genUuid :: QuickCheck.Gen Uuid.UUID
genUuid = Uuid.fromWords64 <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary
