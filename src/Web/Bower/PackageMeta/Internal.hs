{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
-- <https://github.com/bower/bower.json-spec>.

module Web.Bower.PackageMeta.Internal where

import Control.Applicative
import Control.Monad
import Control.Category ((>>>))
import Control.Monad.Error.Class (MonadError(..))
import Data.Monoid
import Data.List
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.BetterErrors

---------------------
-- Data types

-- | A data type representing the data stored in a bower.json package manifest
-- file.
--
-- Note that the 'ToJSON' / 'FromJSON' instances don't exactly match; for
-- example, it is not always the case that decoding from JSON and then encoding
-- to JSON will give you the exact same JSON that you started with. However, if
-- you start with a PackageMeta value, encode to JSON, and then decode, you
-- should always get the same value back.
data PackageMeta = PackageMeta
  { bowerName            :: PackageName
  , bowerDescription     :: Maybe String
  , bowerMain            :: [FilePath]
  , bowerModuleType      :: [ModuleType]
  , bowerLicense         :: [String]
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

-- | A valid package name for a Bower package.
newtype PackageName
  = PackageName String
  deriving (Show, Eq, Ord)

runPackageName :: PackageName -> String
runPackageName (PackageName s) = s

-- | A smart constructor for a PackageName. It ensures that the package name
-- satisfies the restrictions described at
-- <https://github.com/bower/bower.json-spec#name>.
mkPackageName :: String -> Either PackageNameError PackageName
mkPackageName = fmap PackageName . validateAll validators
  where
  dashOrDot = ['-', '.']
  validateAll vs x = mapM_ (validateWith x) vs >> return x
  validateWith x (p, err)
    | p x       = Right x
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

data Author = Author
  { authorName     :: String
  , authorEmail    :: Maybe String
  , authorHomepage :: Maybe String
  }
  deriving (Show, Eq, Ord)

-- | See: <https://github.com/bower/bower.json-spec#moduletype>
data ModuleType
  = Globals
  | AMD
  | Node
  | ES6
  | YUI
  deriving (Show, Eq, Ord, Enum, Bounded)

moduleTypes :: [(String, ModuleType)]
moduleTypes = map (\t -> (map toLower (show t), t)) [minBound .. maxBound]

data Repository = Repository
  { repositoryUrl :: String
  , repositoryType :: String
  }
  deriving (Show, Eq, Ord)

newtype Version
  = Version { runVersion :: String }
  deriving (Show, Eq, Ord)

newtype VersionRange
  = VersionRange { runVersionRange :: String }
  deriving (Show, Eq, Ord)

data BowerError
  = InvalidPackageName PackageNameError
  | InvalidModuleType String
  deriving (Show, Eq, Ord)

showBowerError :: BowerError -> Text
showBowerError (InvalidPackageName err) =
  "Invalid package name: " <> showPackageNameError err
showBowerError (InvalidModuleType str) =
  "Invalid module type: " <> T.pack str <>
    ". Must be one of: " <> renderList moduleTypes
  where
  renderList =
    map (T.pack . show . fst)
      >>> T.intercalate ", "

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator
  deriving (Show, Eq, Ord)

showPackageNameError :: PackageNameError -> Text
showPackageNameError err = case err of
  NotEmpty ->
    "A package name may not be empty"
  TooLong x ->
    "Package names must be no more than 50 characters, yours was " <>
      T.pack (show x)
  InvalidChars str ->
    "The following characters are not permitted in package names: " <>
      T.intercalate " " (map T.singleton str)
  RepeatedSeparators ->
    "The substrings \"--\" and \"..\" may not appear in "<>
      "package names"
  MustNotBeginSeparator ->
    "Package names may not begin with a dash or a dot"
  MustNotEndSeparator ->
    "Package names may not end with a dash or a dot"

displayError :: ParseError BowerError -> Text
displayError = T.unlines . Data.Aeson.BetterErrors.displayError showBowerError

-------------------------
-- Parsing

-- | Read and attempt to decode a bower.json file.
decodeFile :: FilePath -> IO (Either (ParseError BowerError) PackageMeta)
decodeFile = fmap (parse asPackageMeta) . B.readFile

-- | A parser for bower.json files, using the aeson-better-errors package.
asPackageMeta :: Parse BowerError PackageMeta
asPackageMeta =
  PackageMeta <$> key "name" (withString parsePackageName)
            <*> keyMay "description" asString
            <*> keyOrDefault "main"       [] (eachInArray asString)
            <*> keyOrDefault "moduleType" [] (eachInArray (withString parseModuleType))
            <*> keyOrDefault "license"    [] (eachInArray asString)
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
    eachInObjectWithKey (parsePackageName . T.unpack) (g <$> asString)

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

asAuthorObject :: Parse e Author
asAuthorObject =
  Author <$> key "name" asString
         <*> keyMay "email" asString
         <*> keyMay "homepage" asString

asRepository :: Parse e Repository
asRepository =
  Repository <$> key "url" asString
             <*> key "type" asString

------------------------
-- Serializing

instance A.ToJSON PackageMeta where
  toJSON PackageMeta{..} =
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
      toText = T.pack . runPackageName

      assoc :: A.ToJSON a => Text -> [(PackageName, a)] -> [Aeson.Pair]
      assoc = maybeArrayAssocPair toText

instance A.ToJSON PackageName where
  toJSON = A.toJSON . runPackageName

instance A.ToJSON ModuleType where
  toJSON = A.toJSON . map toLower . show

instance A.ToJSON Repository where
  toJSON Repository{..} =
    A.object [ "url" .= repositoryUrl
             , "type" .= repositoryType
             ]

instance A.ToJSON Author where
  toJSON Author{..} =
    A.object $
      [ "name" .= authorName ] ++
        maybePair "email" authorEmail ++
        maybePair "homepage" authorHomepage

instance A.ToJSON Version where
  toJSON = A.toJSON . runVersion

instance A.ToJSON VersionRange where
  toJSON = A.toJSON . runVersionRange

maybePair :: A.ToJSON a => Text -> Maybe a -> [Aeson.Pair]
maybePair k = maybe [] (\val -> [k .= val])

maybeArrayPair :: A.ToJSON a => Text -> [a] -> [Aeson.Pair]
maybeArrayPair _   [] = []
maybeArrayPair k xs = [k .= xs]

maybeArrayAssocPair :: A.ToJSON b => (a -> Text) -> Text -> [(a,b)] -> [Aeson.Pair]
maybeArrayAssocPair _ _   [] = []
maybeArrayAssocPair f k xs = [k .= A.object (map (\(k', v) -> f k' .= v) xs)]

-------------------------
-- FromJSON instances

instance A.FromJSON PackageMeta where
  parseJSON = toAesonParser showBowerError asPackageMeta

instance A.FromJSON PackageName where
  parseJSON = toAesonParser showBowerError (withString parsePackageName)

instance A.FromJSON ModuleType where
  parseJSON = toAesonParser showBowerError (withString parseModuleType)

instance A.FromJSON Repository where
  parseJSON = toAesonParser' asRepository

instance A.FromJSON Author where
  parseJSON = toAesonParser' asAuthor

instance A.FromJSON Version where
  parseJSON = toAesonParser' (Version <$> asString)

instance A.FromJSON VersionRange where
  parseJSON = toAesonParser' (VersionRange <$> asString)
