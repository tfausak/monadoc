{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Key where

import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Key a
  = Key Int.Int64
  deriving (Eq, Ord, Show)

instance Witch.From Int.Int64 (Key a)

instance Witch.From (Key a) Int.Int64

instance Sql.FromField (Key a) where
  fromField = fmap (Witch.from @Int.Int64) . Sql.fromField

instance Sql.ToField (Key a) where
  toField = Sql.toField . Witch.into @Int.Int64

instance Witch.From Int (Key a) where
  from = Witch.via @Int.Int64

instance QuickCheck.Arbitrary (Key a) where
  arbitrary = Witch.from @Int.Int64 <$> QuickCheck.arbitrary

instance Witch.From (Key a) Text.Text where
  from = Witch.from . show . Witch.into @Int.Int64

zero :: Key a
zero = Witch.from @Int 0
