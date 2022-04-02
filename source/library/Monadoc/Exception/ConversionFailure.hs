module Monadoc.Exception.ConversionFailure where

import qualified Control.Monad.Catch as Exception
import qualified Data.Typeable as Typeable
import qualified Witch

newtype ConversionFailure source target
  = ConversionFailure (Witch.TryFromException source target)
  deriving (Show)

instance
  ( Show source,
    Typeable.Typeable source,
    Typeable.Typeable target
  ) =>
  Exception.Exception (ConversionFailure source target)

new :: forall target source. source -> ConversionFailure source target
new = ConversionFailure . flip Witch.TryFromException Nothing
