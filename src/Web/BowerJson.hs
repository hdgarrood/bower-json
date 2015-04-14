{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
-- <https://github.com/bower/bower.json-spec>.

module Web.BowerJson
  ( BowerJson(..)
  , decodeFile
  , PackageName
  , runPackageName
  , mkPackageName
  , ModuleType(..)
  , Author(..)
  , Repository(..)
  , VersionRange(..)
  , Version(..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Category ((>>>))
import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap

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
  , bowerDependencies    :: [(PackageName, VersionRange)]
  , bowerDevDependencies :: [(PackageName, VersionRange)]
  , bowerResolutions     :: [(PackageName, Version)]
  , bowerPrivate         :: Bool
  }
  deriving (Show, Eq, Ord)

instance FromJSON BowerJson where
  parseJSON =
    withObject "BowerJson" $ \o ->
      BowerJson <$> (o .: "name" >>= parsePackageName)
                <*> o .:? "description"
                <*> o .:? "main"             .!= []
                <*> o .:? "moduleType"       .!= []
                <*> o .:? "licence"          .!= []
                <*> o .:? "ignore"           .!= []
                <*> o .:? "keywords"         .!= []
                <*> o .:? "authors"          .!= []
                <*> o .:? "homepage"
                <*> o .:? "repository"
                <*> parseAssocList o "dependencies"
                <*> parseAssocList o "devDependencies"
                <*> parseAssocList o "resolutions"
                <*> o .:? "private"          .!= False
    where
    liftMaybe :: String -> (a -> Maybe b) -> a -> Aeson.Parser b
    liftMaybe ty f =
      maybe (fail ("unable to parse a value of type: " ++ ty)) return . f

    parsePackageName :: String -> Aeson.Parser PackageName
    parsePackageName = liftMaybe "PackageName" mkPackageName

    parseAssocList :: FromJSON v =>
      Aeson.Object -> Text -> Aeson.Parser [(PackageName, v)]
    parseAssocList o k = 
      o .:? k .!= HashMap.empty
        >>= assocListFromObject (parsePackageName . T.unpack)

assocListFromObject :: FromJSON v =>
  (Text -> Aeson.Parser a) ->
  Aeson.Object ->
  Aeson.Parser [(a,v)]
assocListFromObject parseKey o = do
  let xs = HashMap.toList o
  mapM (\(k, v) -> (,) <$> parseKey k <*> parseJSON v) xs

-- | Read and attempt to decode a bower.json file.
decodeFile :: FilePath -> IO (Either String BowerJson)
decodeFile = fmap eitherDecode . B.readFile

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName String
  deriving (Show, Eq, Ord)

runPackageName :: PackageName -> String
runPackageName (PackageName s) = s

instance FromJSON PackageName where
  parseJSON =
    withText "PackageName" $ \text ->
      case mkPackageName (T.unpack text) of
        Just pkgName -> return pkgName
        Nothing -> fail ("unable to validate package name: " ++ show text)

-- | A smart constructor for a PackageName. It ensures that the package name
-- satisfies the restrictions described at
-- <https://github.com/bower/bower.json-spec#name>.
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
      , not . isInfixOf "--"
      , not . isInfixOf ".."
      , length >>> (<= 50)
      ]
  isJustAnd = maybe False

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (_:xs) = lastMay xs

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
  parseJSON v =
    Aeson.typeMismatch "Author" v

-- | Given a prefix and a suffix, go through the supplied list, attempting
-- to extract one string from the list which has the given prefix and suffix,
-- All other strings in the list are returned as the second component of the
-- tuple.
takeDelim :: String -> String -> [String] -> (Maybe String, [String])
takeDelim start end = foldr go (Nothing, [])
  where
  go str (Just x, strs) =
    (Just x, str : strs)
  go str (Nothing, strs) =
    case stripWrapper start end str of
      Just str' -> (Just str', strs)
      Nothing   -> (Nothing, str : strs)

-- | Like stripPrefix, but strips a suffix as well.
stripWrapper :: String -> String -> String -> Maybe String
stripWrapper start end =
  stripPrefix start
    >>> fmap reverse
    >=> stripPrefix (reverse end)
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
