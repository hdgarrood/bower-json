{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aeson.BetterErrors.Test where

import Control.Applicative hiding ((<|>))
import Control.Monad.Error.Class
import qualified Data.Text as T

import Data.Aeson.BetterErrors
import Web.BowerJson

data Person = Person String Int deriving (Show)

asPerson :: Parse () Person
asPerson =
  Person <$> key "name" asString
         <*> key "age" asIntegral

p1 = runParse asPerson

asTuple :: Parse () (String, Int)
asTuple =
  (,) <$> nth 0 asString
      <*> nth 1 asIntegral

p2 = runParse asTuple

data Class = Class
  { classTeacher :: Person
  , classStudents :: [Person]
  }
  deriving (Show)

asClass :: Parse () Class
asClass =
  Class <$> key "teacher" asPerson
        <*> key "children" (eachInArray asPerson)

asBowerJson :: Parse String BowerJson
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
  f g = maybe (Left "Fuck you") Right . g
  parseModuleType = f (flip lookup moduleTypes)

  parsePackageName :: String -> Either String PackageName
  parsePackageName = f mkPackageName

  asAssocListOf :: (String -> a) -> Parse String [(PackageName, a)]
  asAssocListOf g =
    eachInObject asString
      >>= mapM ((\(k,v) ->
        liftEither ((,) <$> parsePackageName (T.unpack k) <*> pure (g v))))

asAuthor :: Parse String Author
asAuthor = catchError parseAuthorString (const parseAuthorObject)
  where
  parseAuthorString = withString $ \s ->
    let (email, s1)    = takeDelim "<" ">" (words s)
        (homepage, s2) = takeDelim "(" ")" s1
    in pure (Author (unwords s2) email homepage)

  parseAuthorObject =
    Author <$> key "name" asString
           <*> keyMay "email" asString
           <*> keyMay "homepage" asString

asRepository =
  Repository <$> key "url" asString
             <*> key "type" asString
