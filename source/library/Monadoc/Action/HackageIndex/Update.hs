{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Action.HackageIndex.Update where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.HackageIndex.Insert as HackageIndex.Insert
import qualified Monadoc.Action.Log as Log
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Exception.Mismatch as Mismatch
import qualified Monadoc.Exception.MissingHeader as MissingHeader
import qualified Monadoc.Exception.NotFound as NotFound
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Model as Model
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run :: HackageIndex.Model -> App.App ()
run hackageIndex = do
  Log.debug "updating hackage index"
  (oldKey, oldSize) <- do
    rows <-
      MonadSql.query
        "select blob.key, blob.size \
        \ from hackageIndex \
        \ inner join blob \
        \ on blob.key = hackageIndex.blob \
        \ where hackageIndex.key = ?"
        [Model.key hackageIndex]
    case rows of
      [] -> Exception.throwM NotFound.NotFound
      (key, size) : _ -> pure (key :: Blob.Key, size)
  newSize <- HackageIndex.Insert.getSize
  let start = oldSize - 1024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString $ "bytes=" <> show start <> "-" <> show end
  case compare oldSize newSize of
    GT ->
      Exception.throwM
        Mismatch.Mismatch
          { Mismatch.expected = oldSize,
            Mismatch.actual = newSize
          }
    EQ -> Log.debug "nothing to update"
    LT -> do
      context <- Reader.ask
      Log.debug $ "new index to get: " <> Text.pack (show $ newSize - oldSize)
      request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar"
      let headers = (Http.hRange, range) : Client.requestHeaders request
      MonadHttp.withResponse request {Client.requestHeaders = headers} $ \response -> do
        actualSize <- getActualSize response
        Monad.when (actualSize /= newSize) . Exception.throwM $
          Mismatch.Mismatch
            { Mismatch.expected = newSize,
              Mismatch.actual = actualSize
            }
        newKey <- HackageIndex.Insert.insertBlob newSize
        MonadSql.withConnection $ \connection -> do
          Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 newKey) True $ \newBlob -> do
            Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 oldKey) False $ \oldBlob -> do
              Log.debug "copying old blob"
              contents <- Base.liftBase $ Sqlite.blobRead oldBlob oldSize 0
              Base.liftBase $ Sqlite.blobWrite newBlob contents 0
              let loop offset = do
                    chunk <- Client.brRead $ Client.responseBody response
                    let size = ByteString.length chunk
                    Monad.when (not $ ByteString.null chunk) $ do
                      Sqlite.blobWrite newBlob chunk offset
                      loop $ offset + size
              Log.debug "appending new blob"
              Base.liftBase $ loop start
          Log.debug "updating new hash"
          Sqlite.withBlobLifted (Sql.connectionHandle connection) "main" "blob" "contents" (Witch.into @Int.Int64 newKey) False $ \blob -> do
            contents <- Base.liftBase $ Sqlite.blobRead blob newSize 0
            MonadSql.execute "update blob set hash = ? where key = ?" (Hash.new contents, newKey)
        Monad.void $ HackageIndex.Insert.insertHackageIndex newKey

getActualSize :: Exception.MonadThrow m => Client.Response a -> m Int
getActualSize response = do
  a <- case lookup Header.contentRange $ Client.responseHeaders response of
    Nothing -> Exception.throwM $ MissingHeader.MissingHeader Header.contentRange
    Just c -> pure c
  b <- Either.throw . Witch.tryInto @String . ByteString.drop 1 . snd $ ByteString.break (== 0x2f) a
  Either.throw $ Read.tryRead b
