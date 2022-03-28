{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Extra.Witch where

import qualified Data.ByteString as ByteString
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Network.HTTP.Types as Http
import qualified Witch

instance Witch.TryFrom ByteString.ByteString Http.StdMethod where
  tryFrom = Witch.maybeTryFrom $ either (const Nothing) Just . Http.parseMethod

instance Witch.TryFrom String Cabal.PackageVersionConstraint where
  tryFrom = Witch.maybeTryFrom Cabal.simpleParsec
