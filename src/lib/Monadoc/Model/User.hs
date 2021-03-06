module Monadoc.Model.User where

import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
import qualified Monadoc.Vendor.Sql as Sql
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Type.GithubId as GithubId
import qualified Monadoc.Type.GithubLogin as GithubLogin
import qualified Monadoc.Type.GithubToken as GithubToken
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model

type Model = Model.Model User

type Key = Key.Key User

data User = User
    { createdAt :: Time.UTCTime
    , deletedAt :: Maybe Time.UTCTime
    , githubId :: GithubId.GithubId
    , githubLogin :: GithubLogin.GithubLogin
    , githubToken :: GithubToken.GithubToken
    , updatedAt :: Time.UTCTime
    } deriving (Eq, Show)

instance Sql.FromRow User where
    fromRow = User
        <$> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field
        <*> Sql.field

instance Sql.ToRow User where
    toRow user =
        [ Sql.toField $ createdAt user
        , Sql.toField $ deletedAt user
        , Sql.toField $ githubId user
        , Sql.toField $ githubLogin user
        , Sql.toField $ githubToken user
        , Sql.toField $ updatedAt user
        ]

migrations :: [Migration.Migration]
migrations =
    [ Migration.new 2021 5 23 18 21 0
        "create table user \
        \(key integer not null primary key, \
        \createdAt text not null, \
        \deletedAt text, \
        \githubId integer, \
        \githubLogin text not null, \
        \githubToken text not null, \
        \updatedAt text not null)"
    , Migration.new 2021 6 12 9 33 0
        "create unique index user_githubId on user (githubId)"
    ]

insertOrUpdate :: Sql.Connection -> User -> IO ()
insertOrUpdate connection =
    Sql.execute
        connection
            "insert into user \
            \(createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt) \
            \values (?, ?, ?, ?, ?, ?) \
            \on conflict (githubId) do update \
            \set deletedAt = excluded.deletedAt, \
            \githubLogin = excluded.githubLogin, \
            \githubToken = excluded.githubToken, \
            \updatedAt = excluded.updatedAt"

selectByGithubId :: Sql.Connection -> GithubId.GithubId -> IO (Maybe Model)
selectByGithubId c i = fmap Maybe.listToMaybe $ Sql.query c
    "select key, createdAt, deletedAt, githubId, githubLogin, githubToken, updatedAt \
    \from user \
    \where deletedAt is null \
    \and githubId = ? \
    \limit 1" [i]
