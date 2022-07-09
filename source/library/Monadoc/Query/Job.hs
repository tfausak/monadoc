{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Job where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Status as Status

selectAll :: App.App [Job.Model]
selectAll = App.query_ "select * from job"

selectByStatus :: Status.Status -> App.App (Maybe Job.Model)
selectByStatus status = do
  rows <-
    App.query
      "select * from job where status = ? order by createdAt asc limit 1"
      [status]
  pure $ Maybe.listToMaybe rows
