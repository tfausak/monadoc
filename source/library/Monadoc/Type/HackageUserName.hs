module Monadoc.Type.HackageUserName where

import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid
import qualified Witch

newtype HackageUserName
  = HackageUserName String
  deriving (Eq, Show)

instance Witch.From String HackageUserName

instance Witch.From HackageUserName String

instance Sql.FromField HackageUserName where
  fromField = fmap (Witch.from @String) . Sql.fromField

instance Sql.ToField HackageUserName where
  toField = Sql.toField . Witch.into @String

instance Lucid.ToHtml HackageUserName where
  toHtml = Lucid.toHtml . Witch.into @String
  toHtmlRaw = Lucid.toHtmlRaw . Witch.into @String
