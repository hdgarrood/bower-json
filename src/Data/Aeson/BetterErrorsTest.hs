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

