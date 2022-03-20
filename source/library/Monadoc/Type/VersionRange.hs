{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.VersionRange where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.VersionRange as Cabal
import qualified Monadoc.Vendor.Witch as Witch

newtype VersionRange
  = VersionRange Cabal.VersionRange
  deriving (Eq, Show)

instance Witch.From Cabal.VersionRange VersionRange

instance Witch.From VersionRange Cabal.VersionRange

instance Witch.TryFrom String VersionRange where
  tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Cabal.VersionRange) . Cabal.simpleParsec

instance Witch.From VersionRange String where
  from = Cabal.prettyShow . Witch.into @Cabal.VersionRange

instance Sql.FromField VersionRange where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField VersionRange where
  toField = Sql.toField . Witch.into @String
