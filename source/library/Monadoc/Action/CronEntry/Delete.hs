{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.CronEntry.Delete where

import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Guid as Guid

run :: Guid.Guid -> App.App ()
run guid = App.execute "delete from cronEntry where guid = ?" [guid]
