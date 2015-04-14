{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
--   <https://github.com/bower/bower.json-spec>

module Web.BowerJson where

import Control.Applicative
import Control.Monad
import Control.Category ((>>>))
import Data.Traversable (traverse)
import Data.Maybe
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
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

instance FromJSON BowerJson where
  parseJSON =
    withObject "BowerJson" $ \o ->
      BowerJson <$> (o .: "name" >>= parsePackageName)
                <*> o .:?  "description"
                <*> o .:?  "main"             .!= []
                <*> o .:?  "moduleType"       .!= []
                <*> o .:?  "licence"          .!= []
                <*> o .:?  "ignore"           .!= []
                <*> o .:?  "keywords"         .!= []
                <*> o .:?  "authors"          .!= []
                <*> o .:?  "homepage"
                <*> o .:?  "repository"
                <*> o .::? "dependencies"
                <*> o .::? "devDependencies"
                <*> o .::? "resolutions"
                <*> o .:?  "private"          .!= False
    where
    liftMaybe :: String -> (a -> Maybe b) -> a -> Aeson.Parser b
    liftMaybe msg f = maybe (fail msg) return . f

    parsePackageName = liftMaybe "PackageName" mkPackageName

    (.::?) o' field =
      (o' .:? field .!= Object mempty)
        >>= parseWithArbitraryKeys parsePackageName

parseWithArbitraryKeys :: (Ord a, FromJSON v) =>
  (String -> Aeson.Parser a) -> Value -> Aeson.Parser (Map a v)
parseWithArbitraryKeys parseKey v' = do
  list <- M.toList <$> parseJSON v'
  list' <- traverse (\(k, v) -> (,v) <$> parseKey k) list
  return (M.fromList list')

------------------
-- Package names

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName { runPackageName :: String }
  deriving (Show, Eq, Ord)

instance FromJSON PackageName where
  parseJSON =
    withText "PackageName" $ \text ->
      case mkPackageName (T.unpack text) of
        Just pkgName -> return pkgName
        Nothing -> fail ("unable to validate package name: " ++ show text)

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
  deriving (Show, Eq, Ord, Enum)

moduleTypes :: [(String, ModuleType)]
moduleTypes = map (\t -> (map toLower (show t), t)) [Globals .. YUI]

instance FromJSON ModuleType where
  parseJSON =
    withText "ModuleType" $ \t ->
      case lookup (T.unpack t) moduleTypes of
        Just t' -> return t'
        Nothing -> fail ("invalid module type: " ++ show t)

data Repository = Repository
  { repositoryUrl :: String
  , repositoryType :: String
  }
  deriving (Show, Eq, Ord)

instance FromJSON Repository where
  parseJSON =
    withObject "Repository" $ \o ->
      Repository <$> o .: "url"
                 <*> o .: "type"

data Author = Author
  { authorName     :: String
  , authorEmail    :: Maybe String
  , authorHomepage :: Maybe String
  }
  deriving (Show, Eq, Ord)

instance FromJSON Author where
  parseJSON (Object o) =
    Author <$> o .: "name"
           <*> o .:? "email"
           <*> o .:? "homepage"
  parseJSON (String t) =
    pure (Author (unwords s2) email homepage)
    where
    (email, s1)    = takeDelim "<" ">" (words (T.unpack t))
    (homepage, s2) = takeDelim "(" ")" s1

    takeDelim :: String -> String -> [String] -> (Maybe String, [String])
    takeDelim start end = foldr go (Nothing, [])
      where
      go str (Just x, strs) =
        (Just x, str : strs)
      go str (Nothing, strs) =
        case extractInside start end str of
          Just str' -> (Just str', strs)
          Nothing   -> (Nothing, str : strs)

    extractInside :: String -> String -> String -> Maybe String
    extractInside start end =
      stripPrefix start
        >>> fmap reverse
        >=> stripPrefix end
        >>> fmap reverse

newtype Version
  = Version { runVersion :: String }
  deriving (Show, Eq, Ord)

instance FromJSON Version where
  parseJSON =
    withText "Version" (pure . Version . T.unpack)

newtype VersionRange
  = VersionRange { runVersionRange :: String }
  deriving (Show, Eq, Ord)

instance FromJSON VersionRange where
  parseJSON =
    withText "VersionRange" (pure . VersionRange . T.unpack)
