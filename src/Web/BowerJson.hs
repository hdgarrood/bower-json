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
import Control.Monad.Error.Class (MonadError(..))
import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.BetterErrors

import qualified Data.HashMap.Strict as HashMap

---------------------
-- Data types

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


data BowerError
  = InvalidPackageName PackageNameError
  | InvalidModuleType String

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator

-------------------------
-- Parsing

-- | A parser for bower.json files, using the aeson-better-errors package.
asBowerJson :: Parse BowerError BowerJson
asBowerJson =
  BowerJson <$> key "name" (withString parsePackageName)
            <*> keyMay "description" asString
            <*> keyOrDefault "main"       [] (eachInArray asString)
            <*> keyOrDefault "moduleType" [] (eachInArray (withString parseModuleType))
            <*> keyOrDefault "licence"    [] (eachInArray asString)
            <*> keyOrDefault "ignore"     [] (eachInArray asString)
            <*> keyOrDefault "keywords"   [] (eachInArray asString)
            <*> keyOrDefault "authors"    [] (eachInArray asAuthor)
            <*> keyMay "homepage" asString
            <*> keyMay "repository" asRepository
            <*> keyOrDefault "dependencies"    [] (asAssocListOf VersionRange)
            <*> keyOrDefault "devDependencies" [] (asAssocListOf VersionRange)
            <*> keyOrDefault "resolutions"     [] (asAssocListOf Version)
            <*> keyOrDefault "private" False asBool
  where
  asAssocListOf :: (String -> a) -> Parse BowerError [(PackageName, a)]
  asAssocListOf g =
    eachInObject asString
      >>= mapM ((\(k,v) ->
        liftEither ((,) <$> parsePackageName (T.unpack k) <*> pure (g v))))

parseModuleType :: String -> Either BowerError ModuleType
parseModuleType str =
  case lookup str moduleTypes of
    Nothing -> Left (InvalidModuleType str)
    Just mt -> Right mt

parsePackageName :: String -> Either BowerError PackageName
parsePackageName str =
  case mkPackageName str of
    Left err -> Left (InvalidPackageName err)
    Right n -> Right n

asAuthor :: Parse e Author
asAuthor = catchError asAuthorString (const asAuthorObject)

asAuthorString :: Parse e Author
asAuthorString = withString $ \s ->
  let (email, s1)    = takeDelim "<" ">" (words s)
      (homepage, s2) = takeDelim "(" ")" s1
  in pure (Author (unwords s2) email homepage)

asAuthorObject :: Parse e Author
asAuthorObject =
  Author <$> key "name" asString
         <*> keyMay "email" asString
         <*> keyMay "homepage" asString

asRepository :: Parse e Repository
asRepository =
  Repository <$> key "url" asString
             <*> key "type" asString

instance A.ToJSON BowerJson where
  toJSON BowerJson{..} =
    A.object $ concat
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

      assoc :: A.ToJSON a => Text -> [(PackageName, a)] -> [Aeson.Pair]
      assoc = maybeArrayAssocPair asText

maybePair :: A.ToJSON a => Text -> Maybe a -> [Aeson.Pair]
maybePair key = maybe [] (\val -> [key .= val])

maybeArrayPair :: A.ToJSON a => Text -> [a] -> [Aeson.Pair]
maybeArrayPair _   [] = []
maybeArrayPair key xs = [key .= xs]

maybeArrayAssocPair :: A.ToJSON b => (a -> Text) -> Text -> [(a,b)] -> [Aeson.Pair]
maybeArrayAssocPair _ _   [] = []
maybeArrayAssocPair f key xs = [key .= A.object (map (\(k, v) -> f k .= v) xs)]

-- | Read and attempt to decode a bower.json file.
-- decodeFile :: FilePath -> IO (Either String BowerJson)
-- decodeFile = fmap A.eitherDecode . B.readFile

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName String
  deriving (Show, Eq, Ord)

runPackageName :: PackageName -> String
runPackageName (PackageName s) = s

instance A.FromJSON PackageName where
  parseJSON = undefined

instance A.ToJSON PackageName where
  toJSON = A.toJSON . runPackageName

-- | A smart constructor for a PackageName. It ensures that the package name
-- satisfies the restrictions described at
-- <https://github.com/bower/bower.json-spec#name>.
mkPackageName :: String -> Either PackageNameError PackageName
mkPackageName = fmap PackageName . validateAll validators
  where
  dashOrDot = ['-', '.']
  validateAll vs x = mapM_ (validateWith x) vs >> return x
  validateWith x (pred, err)
    | pred x    = Right x
    | otherwise = Left (err x)
  validChar c = isAscii c && (isLower c || isDigit c || c `elem` dashOrDot)
  validators =
      [ (not . null, const NotEmpty)
      , (all validChar, InvalidChars . filter (not . validChar))
      , (headMay >>> isJustAnd (`notElem` dashOrDot), const MustNotBeginSeparator)
      , (lastMay >>> isJustAnd (`notElem` dashOrDot), const MustNotEndSeparator)
      , (not . isInfixOf "--", const RepeatedSeparators)
      , (not . isInfixOf "..", const RepeatedSeparators)
      , (length >>> (<= 50), TooLong . length)
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

instance A.FromJSON ModuleType where
  parseJSON = undefined

instance A.ToJSON ModuleType where
  toJSON = A.toJSON . map toLower . show

data Repository = Repository
  { repositoryUrl :: String
  , repositoryType :: String
  }
  deriving (Show, Eq, Ord)

instance A.FromJSON Repository where
  parseJSON = undefined

instance A.ToJSON Repository where
  toJSON Repository{..} =
    A.object [ "url" .= repositoryUrl
             , "type" .= repositoryType
             ]

data Author = Author
  { authorName     :: String
  , authorEmail    :: Maybe String
  , authorHomepage :: Maybe String
  }
  deriving (Show, Eq, Ord)

instance A.FromJSON Author where
  parseJSON = undefined

instance A.ToJSON Author where
  toJSON Author{..} =
    A.object $
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

instance A.FromJSON Version where
  parseJSON = undefined

instance A.ToJSON Version where
  toJSON = A.toJSON . runVersion

newtype VersionRange
  = VersionRange { runVersionRange :: String }
  deriving (Show, Eq, Ord)

instance A.FromJSON VersionRange where
  parseJSON = undefined

instance A.ToJSON VersionRange where
  toJSON = A.toJSON . runVersionRange
