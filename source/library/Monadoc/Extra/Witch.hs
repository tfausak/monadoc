{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Monadoc.Extra.Witch where

import qualified Data.CaseInsensitive as CI
import qualified Witch

instance CI.FoldCase a => Witch.From a (CI.CI a) where
  from = CI.mk

instance Witch.From (CI.CI a) a where
  from = CI.original
