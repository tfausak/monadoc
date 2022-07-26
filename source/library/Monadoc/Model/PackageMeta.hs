module Monadoc.Model.PackageMeta where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version
import qualified Monadoc.Type.BuildType as BuildType
import qualified Monadoc.Type.Hash as Hash
import qualified Monadoc.Type.Key as Key
import qualified Monadoc.Type.Model as Model
import qualified Test.QuickCheck as QuickCheck

type Key = Key.Key PackageMeta

type Model = Model.Model PackageMeta

data PackageMeta = PackageMeta
  { buildType :: BuildType.BuildType,
    cabalVersion :: Version.Key,
    hash :: Hash.Hash,
    license :: License.Key,
    upload :: Upload.Key,
    -- | The author field is most often empty. It is completely free form.
    -- However when it's not empty, it's usually one of these forms:
    --
    -- - Full name: @"Maxine Mayfield"@.
    -- - Name with email: @"Joyce Byers <joyce@example>"@.
    -- - Multiple names: @"Nancy Wheeler and Jane Hopper"@. Sometimes there are
    --   many names. Separated by conjunctions and/or commas. May also include
    --   email addresses.
    -- - Hackage username: @"MadMax"@.
    -- - Company name: @"Melvald's General Store"@
    -- - File path: @"AUTHORS"@
    author :: Maybe Text.Text,
    -- | The bug reports field is most often empty. It is completely free form.
    -- However when it's not empty, it's usually one of these forms:
    --
    -- - URL: @"https://github.example/hawkins/lab/issues"@. This is often
    --   GitHub, but not exclusively.
    -- - Email: @"mailto:brenner@example"@. May or may not include the
    --   @mailto:@ prefix.
    bugReports :: Maybe Text.Text,
    -- | The category field is sometimes empty. Typically it is a
    -- comma-separated list of category names. Category names are completely
    -- free form, but are typically title case phrases like @"Web"@ or
    -- @"Data Structures"@.
    category :: Maybe Text.Text,
    -- | The copyright field is most often empty. It is completely free form.
    -- However when it's not empty, it's usually one of these forms:
    --
    -- - A name, like the 'author' field.
    -- - Some permutation of @"Copyright: (c) 2016-2022 The Duffer Brothers@".
    --   Each section, for lack of a better term, is optional and can be
    --   formatted slightly differently.
    copyright :: Maybe Text.Text,
    -- | The description field is most often empty. When it's not empty, it's a
    -- free form document using Haddock syntax. Often it will mention another
    -- file, typically @README.md@.
    description :: Maybe Text.Text,
    -- | The homepage field is most often empty. When it's not empty, it's
    -- nearly always a URL. Often this is similar to the 'bugReports' field.
    homepage :: Maybe Text.Text,
    -- | The maintainer field is nominally required, although it is sometimes
    -- empty. It is similar to the 'author' field and often has the same value.
    maintainer :: Maybe Text.Text,
    -- | The package URL field is most often empty. It's similar to the
    -- 'homepage' field and often has the same value.
    pkgUrl :: Maybe Text.Text,
    -- | The stability field is most often empty. When it's not empty, it's
    -- typically a single word like @"Experimental"@ or @"stable"@. However
    -- it's completely free form and often contains arbitrary descriptions.
    stability :: Maybe Text.Text,
    -- | The synopsis field is nominally required, but it's sometimes empty.
    -- It's typically a short, human-readable description of what the package
    -- does. Almost always less than 100 characters, usually around 50. Unlike
    -- the 'description' field, this is plain text (not Haddock).
    synopsis :: Maybe Text.Text
  }
  deriving (Eq, Show)

instance Sql.FromRow PackageMeta where
  fromRow =
    PackageMeta
      <$> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field
      <*> Sql.field

instance Sql.ToRow PackageMeta where
  toRow packageMeta =
    [ Sql.toField $ buildType packageMeta,
      Sql.toField $ cabalVersion packageMeta,
      Sql.toField $ hash packageMeta,
      Sql.toField $ license packageMeta,
      Sql.toField $ upload packageMeta,
      Sql.toField $ author packageMeta,
      Sql.toField $ bugReports packageMeta,
      Sql.toField $ category packageMeta,
      Sql.toField $ copyright packageMeta,
      Sql.toField $ description packageMeta,
      Sql.toField $ homepage packageMeta,
      Sql.toField $ maintainer packageMeta,
      Sql.toField $ pkgUrl packageMeta,
      Sql.toField $ stability packageMeta,
      Sql.toField $ synopsis packageMeta
    ]

instance QuickCheck.Arbitrary PackageMeta where
  arbitrary =
    PackageMeta
      <$> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> QuickCheck.arbitrary
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText
      <*> genMaybeText

genMaybeText :: QuickCheck.Gen (Maybe Text.Text)
genMaybeText = QuickCheck.liftArbitrary genText

genText :: QuickCheck.Gen Text.Text
genText = Text.pack <$> QuickCheck.arbitrary

migrations :: [Migration.Migration]
migrations =
  [ Migration.new
      (2022, 6, 10, 17, 40, 0)
      "create table packageMeta \
      \ ( key integer primary key \
      \ , buildType text not null \
      \ , cabalVersion integer not null references version \
      \ , hash blob not null \
      \ , license integer not null references license \
      \ , upload integer not null references upload unique )",
    Migration.new (2022, 6, 12, 10, 2, 0) "alter table packageMeta add column author text",
    Migration.new (2022, 6, 12, 10, 2, 1) "alter table packageMeta add column bugReports text",
    Migration.new (2022, 6, 12, 10, 2, 2) "alter table packageMeta add column category text",
    Migration.new (2022, 6, 12, 10, 2, 3) "alter table packageMeta add column copyright text",
    Migration.new (2022, 6, 12, 10, 2, 4) "alter table packageMeta add column description text",
    Migration.new (2022, 6, 12, 10, 2, 5) "alter table packageMeta add column homepage text",
    Migration.new (2022, 6, 12, 10, 2, 6) "alter table packageMeta add column maintainer text",
    Migration.new (2022, 6, 12, 10, 2, 7) "alter table packageMeta add column pkgUrl text",
    Migration.new (2022, 6, 12, 10, 2, 8) "alter table packageMeta add column stability text",
    Migration.new (2022, 6, 12, 10, 2, 9) "alter table packageMeta add column synopsis text"
  ]
