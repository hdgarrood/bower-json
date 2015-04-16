{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A utility module for dealing with reading JSON, and generating good error
-- messages in the case of JSON with a bad schema.

module Data.Aeson.BetterErrors where
  -- ( Parse
  -- , Error(..)
  -- , PathPart(..)
  -- ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except

import qualified Data.Aeson as A

import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Scientific (Scientific)
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type Parse err a
  = ReaderT ParseReader (Except (ParseError err)) a
  --deriving (Functor, Applicative, Monad)

runParse :: Parse err a -> BL.ByteString -> Either (ParseError err) a
runParse p str =
  case A.eitherDecode str of
    Left err -> Left (InvalidJSON err)
    Right value ->
      let initialReader = ParseReader [] value
      in  runExcept (runReaderT p initialReader)

data ParseReader = ParseReader
  { rdrPath  :: [PathPiece]
  , rdrValue :: A.Value
  }

-- helper functions for ParseReader
appendPath :: PathPiece -> ParseReader -> ParseReader
appendPath p r = r { rdrPath = rdrPath r ++ [p] }
-- TODO DList

setValue :: A.Value -> ParseReader -> ParseReader
setValue v r = r { rdrValue = v }

data PathPiece
  = ObjectKey Text
  | ArrayIndex Int
  deriving (Show, Eq, Ord)

data ParseError err
  = InvalidJSON String
  | BadSchema [PathPiece] (ErrorSpecifics err)
  deriving (Show, Eq)

data ErrorSpecifics err
  = KeyMissing Text
  | OutOfBounds Int
  | WrongType JSONType A.Value -- ^ Expected type, actual value
  | ExpectedIntegral Double
  | CustomError err
  deriving (Show, Eq)

data JSONType
  = TyObject
  | TyArray
  | TyString
  | TyNumber
  | TyBoolean
  | TyNull
  deriving (Show, Eq, Ord)

jsonTypeOf :: A.Value -> JSONType
jsonTypeOf (A.Object _) = TyObject
jsonTypeOf (A.Array _)  = TyArray
jsonTypeOf (A.String _) = TyString
jsonTypeOf (A.Number _) = TyNumber
jsonTypeOf (A.Bool _)   = TyBoolean
jsonTypeOf A.Null       = TyNull

liftParse :: (A.Value -> Either (ErrorSpecifics err) a) -> Parse err a
liftParse f = do
  value <- asks rdrValue
  case f value of
    Right x -> return x
    Left specifics -> badSchema specifics

-- | Aborts parsing, due to an error in the structure of the JSON - that is,
-- any error other than the JSON not actually being parseable into a 'Value'.
badSchema :: ErrorSpecifics err -> Parse err a
badSchema specifics = do
  path <- asks rdrPath
  throwError (BadSchema path specifics)

-- | Parse a single JSON string as 'Text'.
asText :: Parse err Text
asText = liftParse $ \v ->
  case v of
    A.String t -> Right t
    _ -> Left (WrongType TyString v)

-- | Parse a single JSON string as a 'String'.
asString :: Parse err String
asString = T.unpack <$> asText

-- | Parse a single JSON number as a 'Scientific'.
asScientific :: Parse err Scientific
asScientific = liftParse $ \v ->
  case v of
    A.Number s -> Right s
    _ -> Left (WrongType TyNumber v)

-- | Parse a single JSON number as any 'Integral' type.
asIntegral :: Integral a => Parse err a
asIntegral =
  S.floatingOrInteger <$> asScientific
    >>= either (badSchema . ExpectedIntegral) return

-- | Parse a single JSON number as any 'RealFloat' type.
asRealFloat :: forall a err. RealFloat a => Parse err a
asRealFloat =
  floatingOrInteger <$> asScientific
    >>= either return (return . fromIntegral)
  where
  -- This local declaration is just here to give GHC a hint as to which type
  -- should be used in the case of an Integral (here, we choose Integer, for
  -- safety).
  floatingOrInteger :: Scientific -> Either a Integer
  floatingOrInteger = S.floatingOrInteger

-- | Parse a single JSON boolean as a 'Bool'.
asBoolean :: Parse err Bool
asBoolean = liftParse $ \v ->
  case v of
    A.Bool b -> Right b
    _ -> Left (WrongType TyBoolean v)

-- | Parse a JSON object, as an 'A.Object'.
asObject :: Parse err A.Object
asObject = liftParse $ \v ->
  case v of
    A.Object obj -> Right obj
    _ -> Left (WrongType TyObject v)

-- | Parse a JSON array, as an 'A.Array'.
asArray :: Parse err A.Array
asArray = liftParse $ \v ->
  case v of
    A.Array arr -> Right arr
    _ -> Left (WrongType TyArray v)

-- | Parse a single `null` value.
asNull :: Parse err ()
asNull = liftParse $ \v ->
  case v of
    A.Null -> Right ()
    _ -> Left (WrongType TyNull v)

-- | Take the value corresponding to a given key in the current object.
key :: Text -> Parse err a -> Parse err a
key k p = key' (badSchema (KeyMissing k)) k p

-- | Take the value corresponding to a given key in the current object, or
-- if no property exists with that key, use the supplied default.
keyOrDefault :: Text -> a -> Parse err a -> Parse err a
keyOrDefault k def p = key' (pure def) k p

key' :: Parse err a -> Text -> Parse err a -> Parse err a
key' onMissing k p = do
  v <- asks rdrValue
  case v of
    A.Object obj ->
      case HashMap.lookup k obj of
        Just v' ->
          local (appendPath (ObjectKey k) . setValue v') p
        Nothing ->
          onMissing
    _ ->
      badSchema (WrongType TyObject v)

-- | Take the nth value of the current array.
nth :: Int -> Parse err a -> Parse err a
nth n p = nth' (badSchema (OutOfBounds n)) n p

-- | Take the nth value of the current array, or if no value exists with that
-- index, use the supplied default.
nthOrDefault :: Int -> a -> Parse err a -> Parse err a
nthOrDefault n def p =
  nth' (pure def) n p

nth' :: Parse err a -> Int -> Parse err a -> Parse err a
nth' onMissing n p = do
  v <- asks rdrValue
  case v of
    A.Array vect ->
      case vect !? n of
        Just v' ->
          local (appendPath (ArrayIndex n) . setValue v') p
        Nothing ->
          onMissing
    _ ->
      badSchema (WrongType TyArray v)

eachInArray :: Parse err a -> Parse err [a]
eachInArray p = do
  xs <- zip [0..] . V.toList <$> asArray
  mapM (\(i, x) -> local (appendPath (ArrayIndex i) . setValue x) p) xs


-- | A version of catchJust from "Control.Exception.Base", except for any
-- instance of 'MonadError'.
catchJust :: MonadError e m
  => (e -> Maybe b) -- ^ Predicate to select exceptions
  -> m a -- ^ Computation to run
  -> (b -> m a) -- ^ Handler
  -> m a
catchJust p act handler = catchError act handle
  where
  handle e =
    case p e of
      Nothing -> throwError e
      Just b -> handler b
