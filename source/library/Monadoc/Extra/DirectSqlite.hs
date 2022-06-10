{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Extra.DirectSqlite where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Trans.Control as Control
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite3 as Sqlite
import qualified System.IO.Unsafe as Unsafe

unsafeBlobRead :: Sqlite.Blob -> Int -> Int -> IO [ByteString.ByteString]
unsafeBlobRead blob total offset = do
  let size = min 8192 (total - offset)
  if size > 0
    then Unsafe.unsafeInterleaveIO $ do
      chunk <- Sqlite.blobRead blob size offset
      (chunk :) <$> unsafeBlobRead blob total (offset + size)
    else pure []

withBlob ::
  Sqlite.Database ->
  Text.Text ->
  Text.Text ->
  Text.Text ->
  Int.Int64 ->
  Bool ->
  (Sqlite.Blob -> IO a) ->
  IO a
withBlob database symbolic table column key write =
  Exception.bracket
    ( Sqlite.blobOpen
        database
        symbolic
        table
        column
        key
        write
    )
    Sqlite.blobClose

withBlobLifted ::
  Control.MonadBaseControl IO m =>
  Sqlite.Database ->
  Text.Text ->
  Text.Text ->
  Text.Text ->
  Int.Int64 ->
  Bool ->
  (Sqlite.Blob -> m a) ->
  m a
withBlobLifted d s t c k = Control.liftBaseOp . withBlob d s t c k
