{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
-- <https://github.com/bower/bower.json-spec>.

module Web.BowerJson where
  -- ( BowerJson(..)
  -- , decodeFile
  -- , PackageName
  -- , runPackageName
  -- , mkPackageName
  -- , ModuleType(..)
  -- , moduleTypes
  -- , Author(..)
  -- , Repository(..)
  -- , VersionRange(..)
  -- , Version(..)
  -- ) where

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
--
-- Note that the 'ToJSON' / 'FromJSON' instances don't exactly match; for
-- example, it is not always the case that decoding from JSON and then encoding
-- to JSON will give you the exact same JSON that you started with. However, if
-- you start with a BowerJson value, encode to JSON, and then decode, you
-- should always get the same value back.
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

instance ToJSON BowerJson where
  toJSON BowerJson{..} =
    object $ concat
      [ [ "name" .= bowerName ]
      , maybePair "description" bowerDescription
      , maybeArrayPair "main" bowerMain
      , maybeArrayPair "moduleType" bowerModuleType
      , maybeArrayPair "licence" bowerLicence
      , maybeArrayPair "ignore" bowerIgnore
      , maybeArrayPair "keywords" bowerKeywords
      , maybeArrayPair "authors" bowerAuthors
      , maybePair "homepage" bowerHomepage
      , maybePair "repository" bowerRepository
      , assoc "dependencies" bowerDependencies
      , assoc "devDependencies" bowerDevDependencies
      , assoc "resolutions" bowerResolutions
      , if bowerPrivate then [ "private" .= True ] else []
      ]

      where
      asText = T.pack . runPackageName

      assoc :: ToJSON a => Text -> [(PackageName, a)] -> [Aeson.Pair]
      assoc = maybeArrayAssocPair asText

maybePair :: ToJSON a => Text -> Maybe a -> [Aeson.Pair]
maybePair key = maybe [] (\val -> [key .= val])

maybeArrayPair :: ToJSON a => Text -> [a] -> [Aeson.Pair]
maybeArrayPair _   [] = []
maybeArrayPair key xs = [key .= xs]

maybeArrayAssocPair :: ToJSON b => (a -> Text) -> Text -> [(a,b)] -> [Aeson.Pair]
maybeArrayAssocPair _ _   [] = []
maybeArrayAssocPair f key xs = [key .= object (map (\(k, v) -> f k .= v) xs)]

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

instance ToJSON PackageName where
  toJSON = toJSON . runPackageName

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

instance ToJSON ModuleType where
  toJSON = toJSON . map toLower . show

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

instance ToJSON Repository where
  toJSON Repository{..} =
    object [ "url" .= repositoryUrl
           , "type" .= repositoryType
           ]

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

instance ToJSON Author where
  toJSON Author{..} =
    object $
      [ "name" .= authorName ] ++
        maybePair "email" authorEmail ++
        maybePair "homepage" authorHomepage

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

instance ToJSON Version where
  toJSON = toJSON . runVersion

newtype VersionRange
  = VersionRange { runVersionRange :: String }
  deriving (Show, Eq, Ord)

instance FromJSON VersionRange where
  parseJSON =
    withText "VersionRange" (pure . VersionRange . T.unpack)

instance ToJSON VersionRange where
  toJSON = toJSON . runVersionRange
