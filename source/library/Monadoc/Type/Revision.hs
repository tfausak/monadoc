{-# LANGUAGE MultiParamTypeClasses #-}

module Monadoc.Type.Revision where

import qualified Witch

newtype Revision
  = Revision Int
  deriving (Eq, Show)

instance Witch.From Int Revision

instance Witch.From Revision Int
