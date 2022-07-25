module Monadoc.Model.Migration where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Extra.Time as Time
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Monadoc.Type.Query as Query
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

type Model = Model.Model Migration

type Key = Key.Key Migration

data Migration = Migration
  { createdAt :: Timestamp.Timestamp,
    query :: Query.Query
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

instance QuickCheck.Arbitrary Migration where
  arbitrary =
    Migration
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary

createTable :: Query.Query
createTable =
  "create table if not exists migration \
  \ ( key integer primary key \
  \ , createdAt text not null unique \
  \ , query text not null )"

new ::
  (Time.Year, Time.MonthOfYear, Time.DayOfMonth, Int, Int, Fixed.Pico) ->
  Query.Query ->
  Migration
new (dy, dm, dd, th, tm, ts) q =
  Migration
    { createdAt = Witch.from $ Time.makeUtcTime dy dm dd th tm ts,
      query = q
    }

migrations :: [Migration]
migrations =
  [ new (2022, 1, 1, 0, 0, 0) "select 1"
  ]
