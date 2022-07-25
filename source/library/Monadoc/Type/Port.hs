module Monadoc.Type.Port where

import qualified Monadoc.Extra.Read as Read
import qualified Witch

newtype Port
  = Port Int
  deriving (Eq, Show)

instance Witch.From Int Port

instance Witch.From Port Int

instance Witch.TryFrom String Port where
  tryFrom = Witch.eitherTryFrom $ fmap (Witch.from @Int) . Read.tryRead
