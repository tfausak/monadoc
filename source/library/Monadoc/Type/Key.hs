{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Key where

import qualified Data.Int as Int
import qualified Monadoc.Vendor.SqliteSimple as Sql
import qualified Monadoc.Vendor.Witch as Witch

newtype Key a
  = Key Int.Int64
  deriving (Eq, Ord, Show)

instance Witch.From Int.Int64 (Key a)

instance Witch.From (Key a) Int.Int64

instance Sql.FromField (Key a) where
  fromField = fmap (Witch.from @Int.Int64) . Sql.fromField

instance Sql.ToField (Key a) where
  toField = Sql.toField . Witch.into @Int.Int64
