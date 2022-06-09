module Monadoc.Constant.Migration where

import qualified Data.List as List
import qualified Monadoc.Model.Blob as Blob
import qualified Monadoc.Model.CronEntry as CronEntry
import qualified Monadoc.Model.HackageIndex as HackageIndex
import qualified Monadoc.Model.HackageUser as HackageUser
import qualified Monadoc.Model.Job as Job
import qualified Monadoc.Model.License as License
import qualified Monadoc.Model.Migration as Migration
import qualified Monadoc.Model.Package as Package
import qualified Monadoc.Model.Preference as Preference
import qualified Monadoc.Model.Range as Range
import qualified Monadoc.Model.Upload as Upload
import qualified Monadoc.Model.Version as Version

all :: [Migration.Migration]
all =
  List.sortOn Migration.createdAt $
    mconcat
      [ Blob.migrations,
        CronEntry.migrations,
        HackageIndex.migrations,
        HackageUser.migrations,
        License.migrations,
        Job.migrations,
        Migration.migrations,
        Package.migrations,
        Preference.migrations,
        Range.migrations,
        Upload.migrations,
        Version.migrations
      ]
