module Monadoc.Exception.NotFound where

import qualified Control.Monad.Catch as Exception
import qualified Data.Maybe as Maybe
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Extra.Maybe as Maybe

data NotFound
  = NotFound
  deriving (Eq, Show)

instance Exception.Exception NotFound

fromMaybe :: (Exception.MonadThrow m) => Maybe a -> m a
fromMaybe = Either.throw . Maybe.note NotFound

fromList :: (Exception.MonadThrow m) => [a] -> m a
fromList = fromMaybe . Maybe.listToMaybe
