module Monadoc.Handler.HealthCheck.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Exception.Sick as Sick
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.Handler as Handler
import qualified Network.HTTP.Types as Http

handler :: Handler.Handler
handler _ respond = do
  rows <- App.Sql.query_ "select 1"
  Monad.when (rows /= [Sql.Only @Int 1]) $ Traced.throw Sick.Sick
  respond $ Common.statusResponse Http.ok200 [(Http.hCacheControl, "no-cache")]
