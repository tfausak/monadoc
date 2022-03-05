module Monadoc.Type.Context where

import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Type.Config as Config
import qualified Network.HTTP.Client as Client

data Context = Context
  { config :: Config.Config,
    connection :: Sql.Connection,
    manager :: Client.Manager
  }
