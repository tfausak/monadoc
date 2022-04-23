module Monadoc.Action.HackageIndex.Update where

import qualified Control.Monad as Monad
import qualified Control.Monad.Base as Base
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Action.HackageIndex.Insert as HackageIndex.Insert
import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Constant.Header as Header
import qualified Monadoc.Exception.InvalidSize as InvalidSize
import qualified Monadoc.Exception.MissingHeader as MissingHeader
import qualified Monadoc.Extra.DirectSqlite as Sqlite
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Read as Read
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Type.Config as Config
import qualified Monadoc.Type.Context as Context
import qualified Monadoc.Type.Timestamp as Timestamp
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Witch

run ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m
  ) =>
  HackageIndex.Key ->
  Int ->
  m ()
run oldKey oldSize = do
  MonadLog.debug "updating hackage index"
  newSize <- HackageIndex.Insert.getSize
  let start = oldSize - 1024
      end = newSize - 1
      range = Witch.into @ByteString.ByteString $ "bytes=" <> show start <> "-" <> show end
  case compare oldSize newSize of
    GT ->
      Exception.throwM $
        InvalidSize.InvalidSize
          { InvalidSize.old = oldSize,
            InvalidSize.new = newSize
          }
    EQ -> MonadLog.debug "nothing to update"
    LT -> do
      context <- Reader.ask
      MonadLog.debug $ "new index to get: " <> Text.pack (show $ newSize - oldSize)
      request <- Client.parseUrlThrow $ Config.hackage (Context.config context) <> "01-index.tar"
      let headers = (Http.hRange, range) : Client.requestHeaders request
      MonadHttp.withResponse request {Client.requestHeaders = headers} $ \response -> do
        actualSize <- getActualSize response
        Monad.when (actualSize /= newSize) . Exception.throwM $
          InvalidSize.InvalidSize
            { InvalidSize.old = newSize,
              InvalidSize.new = actualSize
            }
        createdAt <- Timestamp.getCurrentTime
        MonadSql.execute "insert into hackageIndex (contents, createdAt, processedAt, size, updatedAt) values (zeroblob(?), ?, null, ?, null)" (newSize, createdAt, newSize)
        newKey <- Key.SelectLastInsert.run
        Pool.withResource (Context.pool context) $ \connection -> do
          Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" (Witch.into @Int.Int64 newKey) True $ \newBlob -> do
            Sqlite.withBlob (Sql.connectionHandle connection) "hackageIndex" "contents" (Witch.into @Int.Int64 oldKey) False $ \oldBlob -> do
              contents <- Base.liftBase $ Sqlite.blobRead oldBlob oldSize 0
              Base.liftBase $ Sqlite.blobWrite newBlob contents 0
              let loop offset = do
                    chunk <- Client.brRead $ Client.responseBody response
                    let size = ByteString.length chunk
                    Monad.when (not $ ByteString.null chunk) $ do
                      Sqlite.blobWrite newBlob chunk offset
                      loop $ offset + size
              Base.liftBase $ loop start
        updatedAt <- Timestamp.getCurrentTime
        MonadSql.execute "update hackageIndex set updatedAt = ? where key = ?" (updatedAt, newKey)

getActualSize :: Exception.MonadThrow m => Client.Response a -> m Int
getActualSize response = do
  a <- case lookup Header.contentRange $ Client.responseHeaders response of
    Nothing -> Exception.throwM $ MissingHeader.MissingHeader Header.contentRange
    Just c -> pure c
  b <- Either.throw . Witch.tryInto @String . ByteString.drop 1 . snd $ ByteString.break (== 0x2f) a
  Either.throw $ Read.tryRead b
