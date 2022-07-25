module Monadoc.Extra.Cabal where

import qualified Control.Monad.Catch as Exception
import qualified Distribution.Parsec as Cabal
import qualified Witch

tryParsec ::
  (Cabal.Parsec t, Witch.From s String) =>
  s ->
  Either (Witch.TryFromException s t) t
tryParsec s = case Cabal.eitherParsec $ Witch.into @String s of
  Left e -> Left . Witch.TryFromException s . Just . Exception.toException $ userError e
  Right t -> Right t
