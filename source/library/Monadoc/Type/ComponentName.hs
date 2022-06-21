{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Monadoc.Type.ComponentName where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Lucid as Html
import qualified Monadoc.Extra.Either as Either
import qualified Monadoc.Type.PackageName as PackageName
import qualified Test.QuickCheck as QuickCheck
import qualified Witch

newtype ComponentName
  = ComponentName Cabal.UnqualComponentName
  deriving (Eq, Show)

instance Witch.From Cabal.UnqualComponentName ComponentName

instance Witch.From ComponentName Cabal.UnqualComponentName

-- | This instance is weird. The 'Cabal.UnqualComponentName' type has a
-- @Parsec@ instance, but that instance is too strict. The Cabal file format
-- unfortunately allows literally anything as a non-library component name.
-- This instance acknowledges that reality while also allowing us to write a
-- more strict parser in the future. See this issue for some details and
-- discussion: <https://github.com/haskell/cabal/issues/7441>.
instance Witch.TryFrom String ComponentName where
  tryFrom = pure . Witch.from . Cabal.mkUnqualComponentName

instance Witch.From ComponentName String where
  from = Cabal.unUnqualComponentName . Witch.into @Cabal.UnqualComponentName

instance Sql.FromField ComponentName where
  fromField field = do
    string <- Sql.fromField @String field
    either (Sql.returnError Sql.ConversionFailed field . show) pure $
      Witch.tryFrom string

instance Sql.ToField ComponentName where
  toField = Sql.toField . Witch.into @String

instance QuickCheck.Arbitrary ComponentName where
  arbitrary = QuickCheck.suchThatMap QuickCheck.arbitrary $ Either.hush . Witch.tryFrom @String

instance Witch.From PackageName.PackageName ComponentName where
  from = Witch.from . Cabal.packageNameToUnqualComponentName . Witch.from

instance Html.ToHtml ComponentName where
  toHtml = Html.toHtml . Witch.into @String
  toHtmlRaw = Html.toHtmlRaw . Witch.into @String
