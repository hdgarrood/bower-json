{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Aeson.BetterErrors.Test where

import Control.Applicative
import Data.Aeson.BetterErrors

data Person = Person String Int deriving (Show)

parsePerson :: Parse () Person
parsePerson =
  Person <$> key "name" asString
         <*> key "age" asIntegral

p1 = runParse parsePerson

parseTuple :: Parse () (String, Int)
parseTuple =
  (,) <$> nth 0 asString
      <*> nth 1 asIntegral

p2 = runParse parseTuple

data Class = Class
  { classTeacher :: Person
  , classStudents :: [Person]
  }
  deriving (Show)

parseClass :: Parse () Class
parseClass =
  Class <$> key "teacher" parsePerson
        <*> key "children" (eachInArray parsePerson)
