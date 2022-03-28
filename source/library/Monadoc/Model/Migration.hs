{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Model.Migration where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model Migration

type Key = Key.Key Migration

data Migration = Migration
  { createdAt :: Time.UTCTime,
    query :: Sql.Query
  }
  deriving (Eq, Show)

instance Sql.FromRow Migration where
  fromRow =
    Migration
      <$> Sql.field
      <*> fmap Sql.Query Sql.field

instance Sql.ToRow Migration where
  toRow migration =
    [ Sql.toField $ createdAt migration,
      Sql.toField . Sql.fromQuery $ query migration
    ]

createTable :: Sql.Query
createTable =
  "create table if not exists migration \
  \ ( key integer primary key \
  \ , createdAt text not null unique \
  \ , query text not null )"

new :: (Integer, Int, Int, Int, Int, Fixed.Pico) -> Sql.Query -> Migration
new (year, month, day, h, m, s) q =
  Migration
    { createdAt =
        Time.UTCTime
          { Time.utctDay = Time.fromGregorian year month day,
            Time.utctDayTime =
              Time.timeOfDayToTime
                Time.TimeOfDay {Time.todHour = h, Time.todMin = m, Time.todSec = s}
          },
      query = q
    }

migrations :: [Migration]
migrations =
  [ new (2022, 3, 11, 0, 0, 0) "select 1"
  ]
