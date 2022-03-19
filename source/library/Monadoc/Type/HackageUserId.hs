{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.HackageUserId where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Witch

newtype HackageUserId
  = HackageUserId Int
  deriving (Eq, Show)

instance Witch.From Int HackageUserId

instance Witch.From HackageUserId Int

instance Sql.FromField HackageUserId where
  fromField = fmap (Witch.from @Int) . Sql.fromField

instance Sql.ToField HackageUserId where
  toField = Sql.toField . Witch.into @Int
