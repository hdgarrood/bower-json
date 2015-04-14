
-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
--   <https://github.com/bower/bower.json-spec>

module Web.BowerJson where

import Control.Applicative
import Control.Category ((>>>))
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import Data.Aeson
import System.FilePath

import Web.BowerJson.Utils

-- | A data type representing the data stored in a bower.json package manifest
-- file.
data BowerJson = BowerJson
  { bowerName            :: PackageName
  , bowerDescription     :: Maybe String
  , bowerMain            :: [FilePath]
  , bowerModuleType      :: [ModuleType]
  , bowerLicence         :: [String]
  , bowerIgnore          :: [String]
  , bowerKeywords        :: [String]
  , bowerAuthors         :: [Author]
  , bowerHomepage        :: Maybe String
  , bowerRepository      :: Maybe Repository
  , bowerDependencies    :: Map PackageName VersionRange
  , bowerDevDependencies :: Map PackageName VersionRange
  , bowerResolutions     :: Map PackageName Version
  , isPrivate            :: Bool
  }
  deriving (Show, Eq, Ord)

------------------
-- Package names

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName { runPackageName :: String }
  deriving (Show, Eq, Ord)

mkPackageName :: String -> Maybe PackageName
mkPackageName str
  | satisfyAll predicates str = Just (PackageName str)
  | otherwise = Nothing
  where
  dashOrDot = ['-', '.']
  satisfyAll ps x = all ($ x) ps
  predicates =
      [ not . null
      , all isAscii -- note: this is necessary because isLower allows Unicode.
      , all (\c -> isLower c || isDigit c || c `elem` dashOrDot)
      , headMay >>> isJustAnd (`notElem` dashOrDot)
      , lastMay >>> isJustAnd (`notElem` dashOrDot)
      ]
  isJustAnd = maybe False

-- | See: <https://github.com/bower/bower.json-spec#moduletype>
data ModuleType
  = Globals
  | AMD
  | Node
  | ES6
  | YUI
  deriving (Show, Eq, Ord)

data Repository = Repository
  { repositoryUrl :: String
  , repositoryType :: String
  }
  deriving (Show, Eq, Ord)

data Author = Author
  { authorName     :: String
  , authorEmail    :: Maybe String
  , authorHomepage :: Maybe String
  }
  deriving (Show, Eq, Ord)

-------------
-- Versions

newtype Version
  = Version { runVersion :: String }
  deriving (Show, Eq, Ord)

newtype VersionRange
  = VersionRange { runVersionRange :: String }
  deriving (Show, Eq, Ord)
