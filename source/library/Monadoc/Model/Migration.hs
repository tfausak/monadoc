module Monadoc.Model.Migration where

import qualified Data.Fixed as Fixed
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Vendor.Witch as Witch

type Model = Model.Model Migration

type Key = Key.Key Migration

data Migration = Migration
  { createdAt :: Time.UTCTime,
    query :: Text.Text
  }
  deriving (Eq, Show)

instance Sql.FromRow Migration where
  fromRow =
    Migration
      <$> Sql.field
      <*> Sql.field

instance Sql.ToRow Migration where
  toRow migration =
    [ Sql.toField $ createdAt migration,
      Sql.toField $ query migration
    ]

createTable :: Sql.Query
createTable =
  Witch.from
    "create table if not exists migration \
    \ ( key integer primary key \
    \ , createdAt text not null unique \
    \ , query text not null )"

new :: (Integer, Int, Int, Int, Int, Fixed.Pico) -> String -> Migration
new (year, month, day, hour, minute, second) string =
  Migration
    { createdAt =
        Time.UTCTime
          { Time.utctDay = Time.fromGregorian year month day,
            Time.utctDayTime =
              Time.timeOfDayToTime
                Time.TimeOfDay
                  { Time.todHour = hour,
                    Time.todMin = minute,
                    Time.todSec = second
                  }
          },
      query = Witch.from string
    }

migrations :: [Migration]
migrations =
  [ new (2022, 3, 11, 0, 0, 0) "select 1"
  ]
