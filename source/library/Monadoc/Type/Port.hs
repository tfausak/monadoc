module Monadoc.Type.Port where

import qualified Text.Read as Read
import qualified Witch

newtype Port
  = Port Int
  deriving (Eq, Show)

instance Witch.From Int Port

instance Witch.From Port Int

instance Witch.TryFrom String Port where
  tryFrom = Witch.maybeTryFrom $ fmap (Witch.from @Int) . Read.readMaybe
