{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.HackageIndex.Upsert where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Control as Control
import qualified Monadoc.Action.HackageIndex.Insert as Insert
import qualified Monadoc.Action.HackageIndex.Update as Update
import qualified Monadoc.Class.MonadHttp as MonadHttp
import qualified Monadoc.Class.MonadLog as MonadLog
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Type.Context as Context

run ::
  ( Control.MonadBaseControl IO m,
    MonadHttp.MonadHttp m,
    MonadLog.MonadLog m,
    Exception.MonadMask m,
    Reader.MonadReader Context.Context m,
    MonadSql.MonadSql m
  ) =>
  m ()
run = do
  MonadLog.debug "upserting hackage index"
  rows <- MonadSql.query_ "select * from hackageIndex order by createdAt desc limit 1"
  case rows of
    [] -> Insert.run
    hackageIndex : _ -> Update.run hackageIndex
