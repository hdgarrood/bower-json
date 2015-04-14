{-# LANGUAGE TupleSections #-}

module Web.BowerJson.Utils where

import Control.Applicative
import Control.Monad
import Control.Category ((>>>))
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Traversable (traverse)
import Data.Aeson
import qualified Data.Aeson.Types as Aeson

--------------
-- Safe

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay [x] = Just x
lastMay (_:xs) = lastMay xs

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

-------------------------
-- String manipulation

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
