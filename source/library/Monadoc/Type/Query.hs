{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Query where

import qualified Data.String as String
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype Query
  = Query Sql.Query
  deriving (Eq, Show)

instance Sql.FromField Query where
  fromField = fmap Witch.from . Sql.fromField @Text.Text

instance Witch.From Text.Text Query where
  from = Witch.from . Sql.Query

instance Witch.From Sql.Query Query

instance Sql.ToField Query where
  toField = Sql.toField @Text.Text . Witch.from

instance Witch.From Query Text.Text where
  from = Sql.fromQuery . Witch.from

instance Witch.From Query Sql.Query

instance String.IsString Query where
  fromString = Witch.from

instance Witch.From String Query where
  from = Witch.via @Text.Text

instance QuickCheck.Arbitrary Query where
  arbitrary = do
    n <- QuickCheck.arbitrary @Int
    pure . Witch.from $ "select " <> show n
