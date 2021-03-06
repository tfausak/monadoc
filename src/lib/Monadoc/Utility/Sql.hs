{-# LANGUAGE TypeApplications #-}

module Monadoc.Utility.Sql where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Exception
import qualified Control.Retry as Retry
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified GHC.Clock as Clock
import qualified Monadoc.Type.RequestId as RequestId
import qualified Monadoc.Utility.Log as Log
import qualified Text.Printf as Printf
import qualified Witch

defaultFromField
    :: ( Sql.FromField s
        , Show s
        , Witch.TryFrom s t
        , Typeable.Typeable s
        , Typeable.Typeable t
        )
    => proxy s
    -> Sql.FieldParser t
defaultFromField p f = do
    s <- Sql.fromField f
    case Witch.tryFrom $ Proxy.asProxyTypeOf s p of
        Left e -> Sql.returnError Sql.ConversionFailed f $ show e
        Right t -> pure t

execute
    :: Sql.ToRow i
    => Sql.Connection
    -> String
    -> i
    -> IO ()
execute connection sql = Monad.void . query @_ @[Sql.SQLData] connection sql

execute_ :: Sql.Connection -> String -> IO ()
execute_ connection sql = execute connection sql ()

query
    :: (Sql.ToRow i, Sql.FromRow o)
    => Sql.Connection
    -> String
    -> i
    -> IO [o]
query connection sql input = do
    requestId <- RequestId.random
    before <- Clock.getMonotonicTime
    result <- Retry.recovering
        Retry.retryPolicyDefault
        [const . Exception.Handler $ pure . (== Sql.ErrorBusy) . Sql.sqlError]
        $ \ retryStatus -> do
            let number = Retry.rsIterNumber retryStatus
            Monad.when (number > 0) . Log.info $ Printf.printf "[sql/%04x] [retry/%d] %s"
                (Witch.into @Word.Word16 requestId)
                number
                (show retryStatus)
            Sql.query connection (Sql.Query $ Witch.into @Text.Text sql) input
    after <- Clock.getMonotonicTime
    Log.info $ Printf.printf "[sql/%04x] %s -- %.3f"
        (Witch.into @Word.Word16 requestId)
        sql
        (after - before)
    pure result

query_ :: Sql.FromRow o => Sql.Connection -> String -> IO [o]
query_ connection sql = query connection sql ()
