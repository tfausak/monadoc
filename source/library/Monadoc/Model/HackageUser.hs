{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.HackageUser where

import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.HackageUserId as HackageUserId
import qualified Monadoc.Type.HackageUserName as HackageUserName
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model HackageUser

type Key = Key.Key HackageUser

data HackageUser = HackageUser
  { id :: HackageUserId.HackageUserId,
    name :: HackageUserName.HackageUserName
  }
  deriving (Eq, Show)

instance Sql.FromRow HackageUser where
  fromRow =
    HackageUser
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow HackageUser where
  toRow hackageUser =
    [ Sql.toField $ Monadoc.Model.HackageUser.id hackageUser,
      Sql.toField $ name hackageUser
    ]

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 3, 18, 1, 0, 0)
      "create table hackageUser \
      \ ( key integer primary key \
      \ , id integer not null unique \
      \ , name text not null unique )"
  ]
