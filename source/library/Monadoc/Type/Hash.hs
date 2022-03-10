{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Hash where

import qualified Crypto.Hash as Crypto
import qualified Monadoc.Vendor.SqliteSimple as Sql
import qualified Monadoc.Vendor.Witch as Witch
import qualified Text.Read as Read

newtype Hash
  = Hash (Crypto.Digest Crypto.SHA256)
  deriving (Eq, Show)

instance Witch.From (Crypto.Digest Crypto.SHA256) Hash

instance Witch.From Hash (Crypto.Digest Crypto.SHA256)

instance Witch.TryFrom String Hash where
  tryFrom =
    Witch.maybeTryFrom $
      fmap (Witch.from @(Crypto.Digest Crypto.SHA256))
        . Read.readMaybe

instance Witch.From Hash String where
  from = show . Witch.into @(Crypto.Digest Crypto.SHA256)

instance Sql.FromField Hash where
  fromField field = do
    string <- Sql.fromField field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom @String string

instance Sql.ToField Hash where
  toField = Sql.toField . Witch.into @String
