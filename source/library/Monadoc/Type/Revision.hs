{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.Revision where

import qualified Witch

newtype Revision
  = Revision Int
  deriving (Eq, Ord, Show)

instance Witch.From Int Revision

instance Witch.From Revision Int

zero :: Revision
zero = Witch.from @Int 0

increment :: Revision -> Revision
increment = Witch.over @Int succ
