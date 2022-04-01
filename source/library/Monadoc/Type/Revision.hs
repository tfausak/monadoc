{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Revision where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Witch

newtype Revision
  = Revision Int
  deriving (Eq, Ord, Show)

instance Witch.From Int Revision

instance Witch.From Revision Int

instance Sql.FromField Revision where
  fromField = fmap (Witch.from @Int) . Sql.fromField

instance Sql.ToField Revision where
  toField = Sql.toField . Witch.into @Int

instance Witch.From Revision String where
  from = show . Witch.into @Int

zero :: Revision
zero = Witch.from @Int 0

increment :: Revision -> Revision
increment = Witch.over @Int succ
