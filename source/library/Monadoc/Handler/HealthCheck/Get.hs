{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.HealthCheck.Get where

import qualified Control.Monad as Monad
import qualified Database.SQLite.Simple as Sql
import qualified Monadoc.Exception.Sick as Sick
import qualified Monadoc.Exception.Traced as Traced
import qualified Monadoc.Handler.Common as Common
import qualified Monadoc.Type.App as App
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  rows <- App.query_ "select 1"
  Monad.when (rows /= [Sql.Only @Int 1]) $ Traced.throw Sick.Sick
  pure $ Common.statusResponse Http.ok200 [(Http.hCacheControl, "no-cache")]
