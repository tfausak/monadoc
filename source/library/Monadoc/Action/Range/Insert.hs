{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Action.Range.Insert where

import qualified Monadoc.Action.Key.SelectLastInsert as Key.SelectLastInsert
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Type.App as App
import qualified Monadoc.Type.Model as Model

run :: Range.Range -> App.App Range.Model
run range = do
  App.execute "insert into range (\"constraint\") values (?)" range
  key <- Key.SelectLastInsert.run
  pure Model.Model {Model.key = key, Model.value = range}
