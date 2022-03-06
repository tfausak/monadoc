{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Extra.SqliteSimple where

import qualified Data.String as String
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Vendor.Witch as Witch

instance Witch.From String Sql.Query where
  from = String.fromString
