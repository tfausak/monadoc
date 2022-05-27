{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Job where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Class.MonadSql as MonadSql
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.Status as Status

selectAll :: MonadSql.MonadSql m => m [Job.Model]
selectAll = MonadSql.query_ "select * from job"

selectByStatus :: MonadSql.MonadSql m => Status.Status -> m (Maybe Job.Model)
selectByStatus status = do
  rows <-
    MonadSql.query
      "select * from job where status = ? order by createdAt asc limit 1"
      [status]
  pure $ Maybe.listToMaybe rows
