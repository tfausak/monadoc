module Monadoc.Type.Breadcrumb where

import qualified Data.Text as Text
import qualified Monadoc.Type.Route as Route

data Breadcrumb = Breadcrumb
  { label :: Text.Text,
    route :: Maybe Route.Route
  }
  deriving (Eq, Show)
