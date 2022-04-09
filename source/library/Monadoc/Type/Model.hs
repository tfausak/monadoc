module Monadoc.Type.Model where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Key as Key
import qualified Test.QuickCheck as QuickCheck

data Model a = Model
  { key :: Key.Key a,
    value :: a
  }
  deriving (Eq, Show)

instance Sql.FromRow a => Sql.FromRow (Model a) where
  fromRow = Model <$> Sql.field <*> Sql.fromRow

instance Sql.ToRow a => Sql.ToRow (Model a) where
  toRow model = Sql.toField (key model) : Sql.toRow (value model)

instance QuickCheck.Arbitrary a => QuickCheck.Arbitrary (Model a) where
  arbitrary =
    Model
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
  shrink model =
    Model
      <$> QuickCheck.shrink (key model)
      <*> QuickCheck.shrink (value model)
