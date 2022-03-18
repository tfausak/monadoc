{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Extra.Witch where

import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Network.HTTP.Types as Http
import qualified Witch

instance CI.FoldCase a => Witch.From a (CI.CI a) where
  from = CI.mk

instance Witch.From (CI.CI a) a where
  from = CI.original

instance Witch.From String Sql.Query where
  from = String.fromString

instance Witch.From Text.Text Sql.Query where
  from = Sql.Query

instance Witch.From String Key.Key where
  from = Key.fromString

instance Witch.TryFrom ByteString.ByteString Http.StdMethod where
  tryFrom = Witch.maybeTryFrom $ either (const Nothing) Just . Http.parseMethod

instance Witch.TryFrom String Cabal.PackageVersionConstraint where
  tryFrom = Witch.maybeTryFrom Cabal.simpleParsec

instance Witch.TryFrom String Cabal.Version where
  tryFrom = Witch.maybeTryFrom Cabal.simpleParsec
