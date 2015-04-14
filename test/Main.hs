{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Monoid
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Web.BowerJson

-- Decode any JSON value, not just arrays/objects.
-- this is a bit of a hack, but the 'proper' way is just too much effort.
decodeValue :: FromJSON a => B.ByteString -> Maybe a
decodeValue = the <=< decode . ("[" <>) . (<> "]")
  where
  the [x] = Just x
  the _ = Nothing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "FromJSON Author instance" authorTests

  ]

authorTests :: [TestTree]
authorTests =
  [ testCase "As string without homepage/email" $ do
      authorWithoutOptionalAttrs @=?
        decodeValue "\"Harry Garrood\""

      -- should not be sensitive to extra whitespace
      authorWithoutOptionalAttrs @=?
        decodeValue "\" Harry Garrood \""

  , testCase "As string with homepage/email" $ do
      authorWithEmail @=?
        decodeValue "\"Harry Garrood <harry@garrood.me>\""

      authorWithHomepage @=?
        decodeValue "\"Harry Garrood (http://harry.garrood.me)\""

      authorWithBoth @=?
        decodeValue "\"Harry Garrood <harry@garrood.me> (http://harry.garrood.me)\""

  , testCase "As object" $ do
      authorWithoutOptionalAttrs @=?
        decode "{\"name\": \"Harry Garrood\"}"

      authorWithEmail @=?
        decode "{\"name\": \"Harry Garrood\", \"email\": \"harry@garrood.me\"}"

      authorWithHomepage @=?
        decode "{\"name\": \"Harry Garrood\", \"homepage\": \"http://harry.garrood.me\"}"

      authorWithBoth @=?
        decode "{\"name\": \"Harry Garrood\", \"email\": \"harry@garrood.me\", \"homepage\": \"http://harry.garrood.me\"}"
  ]

  where
  authorWithoutOptionalAttrs = Just (Author "Harry Garrood" Nothing Nothing)
  authorWithEmail = Just (Author "Harry Garrood" (Just "harry@garrood.me") Nothing)
  authorWithHomepage = Just (Author "Harry Garrood" Nothing (Just "http://harry.garrood.me"))
  authorWithBoth = Just (Author "Harry Garrood" (Just "harry@garrood.me") (Just "http://harry.garrood.me"))
