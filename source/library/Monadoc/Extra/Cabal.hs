module Monadoc.Extra.Cabal where

import qualified Distribution.Parsec as Cabal
import qualified Monadoc.Extra.Either as Either
import qualified Witch

tryParsec ::
  forall t s.
  (Witch.From s String, Cabal.Parsec t) =>
  s ->
  Either (Witch.TryFromException s t) t
tryParsec s =
  Either.note (Witch.TryFromException s Nothing)
    . Cabal.simpleParsec
    $ Witch.into @String s
