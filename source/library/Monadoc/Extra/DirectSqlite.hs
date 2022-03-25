{-# LANGUAGE TypeApplications #-}

module Monadoc.Extra.DirectSqlite where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite3 as Sqlite
import qualified Monadoc.Vendor.Witch as Witch
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
  Int.Int64 ->
  Bool ->
  (Sqlite.Blob -> IO a) ->
  IO a
withBlob database table column key write action = do
  let acquire =
        Sqlite.blobOpen
          database
          (Witch.into @Text.Text "main")
          table
          column
          key
          write
  Exception.bracket acquire Sqlite.blobClose action
