module Monadoc.Handler.HealthCheck.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Exception.Sick as Sick
import qualified Monadoc.Handler.Common as Common
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: (MonadSql.MonadSql m, Exception.MonadThrow m) => Wai.Request -> m Wai.Response
handler _ = do
  rows <- MonadSql.query_ "select 1"
  Monad.when (rows /= [Sql.Only @Int 1]) $ Exception.throwM Sick.Sick
  pure $ Common.statusResponse Http.ok200 [(Http.hCacheControl, "no-cache")]
