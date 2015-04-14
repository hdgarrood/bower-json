{-# LANGUAGE TupleSections #-}

module Web.BowerJson.Utils where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

--------------
-- Safe

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (x:xs) = lastMay xs

----------------
-- Aeson

-- | Aeson only provides FromJSON instances such as: @FromJSON a => FromJSON
-- (Map String a)@. This function allows you to parse a Map value from JSON
-- where the keys are not 'String', when you supply a function @(String ->
-- Parser a)@ to parse the keys with.
parseWithArbitraryKeys :: (Ord a, FromJSON v) =>
  (String -> Aeson.Parser a) -> Value -> Aeson.Parser (Map a v)
parseWithArbitraryKeys parseKey v' = do
  list <- M.toList <$> parseJSON v'
  list' <- traverse (\(k, v) -> (,v) <$> parseKey k) list
  return (M.fromList list')
