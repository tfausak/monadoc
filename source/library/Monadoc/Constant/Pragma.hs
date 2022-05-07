{-# LANGUAGE OverloadedStrings #-}

module Monadoc.Constant.Pragma where

import qualified Database.SQLite.Simple as Sql

all :: [Sql.Query]
all =
  [ "pragma auto_vacuum = 'incremental'",
    "pragma foreign_keys = true",
    "pragma journal_mode = 'wal'"
  ]
