{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Query.HackageUser where

import qualified Data.Maybe as Maybe
import qualified Monadoc.Action.App.Sql as App.Sql
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Type.App as App

selectByKey :: HackageUser.Key -> App.App (Maybe HackageUser.Model)
selectByKey key = do
  hackageUsers <-
    App.Sql.query
      "select * from hackageUser where key = ? limit 1"
      [key]
  pure $ Maybe.listToMaybe hackageUsers
