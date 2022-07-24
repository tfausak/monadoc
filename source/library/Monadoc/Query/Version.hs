{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.Version where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.VersionNumber as VersionNumber

selectByNumber :: VersionNumber.VersionNumber -> App.App (Maybe Version.Model)
selectByNumber versionNumber = do
  versions <-
    App.Sql.query
      "select * from version where number = ? limit 1"
      [versionNumber]
  pure $ Maybe.listToMaybe versions
