{-# LANGUAGE TypeApplications #-}

module Monadoc.Handler.HealthCheck.Get where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Data.Pool as Pool
import qualified Monadoc.Exception.Sick as Sick
import qualified Monadoc.Middleware.HandleExceptions as HandleExceptions
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Vendor.SqliteSimple as Sql
import qualified Monadoc.Vendor.Witch as Witch
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

handler :: Wai.Request -> App.App Wai.Response
handler _ = do
  context <- App.ask
  rows <- Pool.withResource (Context.pool context) $ \connection ->
    App.lift . Sql.query_ connection $
      Witch.into @Sql.Query "select 1"
  Monad.when (rows /= [Sql.Only @Int 1]) $ Exception.throwM Sick.Sick
  pure $ HandleExceptions.statusResponse Http.ok200 []
