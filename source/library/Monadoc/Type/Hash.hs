{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Hash where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as ByteString
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Witch

newtype Hash
  = Hash (Crypto.Digest Crypto.SHA256)
  deriving (Eq, Show)

instance Witch.From (Crypto.Digest Crypto.SHA256) Hash

instance Witch.From Hash (Crypto.Digest Crypto.SHA256)

instance Witch.TryFrom ByteString.ByteString Hash where
  tryFrom =
    Witch.maybeTryFrom $
      fmap Witch.from . Crypto.digestFromByteString @Crypto.SHA256

instance Witch.From Hash ByteString.ByteString where
  from = ByteArray.convert . Witch.into @(Crypto.Digest Crypto.SHA256)

instance Sql.FromField Hash where
  fromField field = do
    byteString <- Sql.fromField @ByteString.ByteString field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom byteString

instance Sql.ToField Hash where
  toField = Sql.toField . Witch.into @ByteString.ByteString
