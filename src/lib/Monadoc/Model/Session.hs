module Monadoc.Model.Session where

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.GithubId as GithubId
import qualified Monadoc.Type.Guid as Guid
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model Session

type Key = Key.Key Session

data Session = Session
    { createdAt :: Time.UTCTime
    , deletedAt :: Maybe Time.UTCTime
    , guid :: Guid.Guid
    , updatedAt :: Time.UTCTime
    , userAgent :: String
    , userGithubId :: GithubId.GithubId
    } deriving (Eq, Show)

instance Sql.FromRow Session where
    fromRow = Session
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow Session where
    toRow session =
        [ Sql.toField $ createdAt session
        , Sql.toField $ deletedAt session
        , Sql.toField $ guid session
        , Sql.toField $ updatedAt session
        , Sql.toField $ userAgent session
        , Sql.toField $ userGithubId session
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 5 23 18 22 0
        "create table session \
        \(key integer not null primary key, \
        \createdAt text not null, \
        \deletedAt text, \
        \guid text not null, \
        \updatedAt text not null, \
        \userAgent text not null, \
        \userGithubId integer not null)"
    , Migration.new 2021 6 12 9 24 0
        "create unique index session_guid on session (guid)"
    ]

insert :: Sql.Connection -> Session -> IO ()
insert connection =
    Sql.execute
        connection
            "insert into session \
            \(createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId) \
            \values (?, ?, ?, ?, ?, ?)"

selectByGuid :: Sql.Connection -> Guid.Guid -> IO (Maybe Model)
selectByGuid c g = fmap Maybe.listToMaybe $ Sql.query c
    "select key, createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId \
    \from session \
    \where deletedAt is null \
    \and guid = ? \
    \limit 1" [g]

selectByGithubId :: Sql.Connection -> GithubId.GithubId -> IO [Model]
selectByGithubId c i = Sql.query c
    "select key, createdAt, deletedAt, guid, updatedAt, userAgent, userGithubId \
    \from session \
    \where userGithubId = ?" [i]

delete :: Sql.Connection -> Key -> IO ()
delete connection key = do
    now <- Time.getCurrentTime
    Sql.execute
        connection
        "update session set deletedAt = ? where key = ?"
        (now, key)
