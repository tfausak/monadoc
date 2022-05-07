{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.HackageUser where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Model = Model.Model HackageUser

type Key = Key.Key HackageUser

newtype HackageUser = HackageUser
  { name :: HackageUserName.HackageUserName
  }
  deriving (Eq, Show)

instance Sql.FromRow HackageUser where
  fromRow =
    HackageUser
      <$> Sql.field

instance Sql.ToRow HackageUser where
  toRow hackageUser =
    [ Sql.toField $ name hackageUser
    ]

instance QuickCheck.Arbitrary HackageUser where
  arbitrary =
    HackageUser
      <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 1, 5, 0, 0, 0)
      "create table hackageUser \
      \ ( key integer primary key \
      \ , name text not null unique )"
  ]
