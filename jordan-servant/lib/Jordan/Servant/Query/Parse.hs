{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Turn your jordan parsers into query-string parsers.
--
-- This module should be considered internal.
-- Import Jordan.Servant.Query instead.
module Jordan.Servant.Query.Parse where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad
import Control.Parallel
import qualified Data.Attoparsec.ByteString as AP
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Functor
import Data.Maybe (mapMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Debug.Trace
import GHC.Generics
import Jordan (parseViaAttoparsec)
import Jordan.FromJSON.Class
import Jordan.FromJSON.Internal.Permutation
import Jordan.Types.Internal.AccumE
import Jordan.Types.JSONError
import Network.HTTP.Types.URI

data QueryKeyComponent
  = RawValue T.Text
  | EmptyBraces
  | BracedValue T.Text
  deriving (Show, Read, Eq, Ord, Generic)

newtype QueryKey = QueryKey {queryKeyComponents :: [QueryKeyComponent]}
  deriving (Show, Read, Eq, Ord, Generic)

labelParser = (AP.<?>)

escapedBrace :: AP.Parser BS.ByteString
escapedBrace = do
  AP.string "]]" `labelParser` "escaped ending brace"
  ("]" <>) <$> afterBrace

afterBrace :: AP.Parser BS.ByteString
afterBrace = do
  taken <- AP.takeWhile (/= 93)
  AP.string "]"
  pure (urlDecode False taken)

escapedBraceEnding :: AP.Parser BS.ByteString
escapedBraceEnding = do
  r <- AP.string "[[" $> "["
  (r <>) <$> unbracedValueInner

unbracedValueInner :: AP.Parser BS.ByteString
unbracedValueInner = do
  keyChars <- AP.takeWhile (/= 91) `labelParser` "until starting brace"
  pure $ urlDecode False keyChars

unbracedValue :: AP.Parser T.Text
unbracedValue = do
  inner <- unbracedValueInner
  case decodeUtf8' inner of
    Left err -> fail (show err)
    Right res
      | res == mempty -> fail "empty unbraced value, invalid"
      | otherwise -> pure res

bracedValue :: AP.Parser T.Text
bracedValue = do
  AP.word8 91 `labelParser` "starting brace"
  after <- afterBrace
  case decodeUtf8' after of
    Left err -> fail $ show err
    Right txt -> pure txt

emptyBraces = do
  AP.string "[]"
  m <- AP.peekWord8
  case m of
    Nothing -> pure EmptyBraces
    Just 93 -> fail "escaped brace"
    Just _ -> pure EmptyBraces

queryComponent :: AP.Parser QueryKeyComponent
queryComponent =
  emptyBraces
    <|> (RawValue <$> unbracedValue)
    <|> (BracedValue <$> bracedValue)

parseQueryKey :: AP.Parser QueryKey
parseQueryKey = QueryKey <$> some queryComponent

newtype QueryParser a = QueryParser
  { runQueryParser ::
      (QueryKey -> Maybe QueryKey) ->
      [(QueryKey, Maybe BS.ByteString)] ->
      Either String (a, [(QueryKey, Maybe BS.ByteString)])
  }

deriving instance Functor QueryParser

instance Applicative QueryParser where
  pure a = QueryParser $ \_ q -> Right (a, q)
  (<*>) = ap

instance Monad QueryParser where
  cb >>= transform = QueryParser $ \read q ->
    case runQueryParser cb read q of
      Left s -> Left s
      Right (a, q') -> runQueryParser (transform a) read q'

-- | Alternative tries the left, than the right.
--
-- Both brances will be sparked off and tried in parallel.
instance Alternative QueryParser where
  empty = QueryParser $ \_ _ -> Left "Alternative.Empty"
  lhs <|> rhs = QueryParser $ \bs q ->
    let lhsR = runQueryParser lhs bs q
        rhsR = runQueryParser rhs bs q
     in rhsR `par` lhsR `pseq` lhsR <|> rhsR

peekTransformedKeyHead :: QueryParser QueryKey
peekTransformedKeyHead = QueryParser $ \transform q ->
  case q of
    [] -> Left "no query elements to peek"
    r@((k, _) : rest) ->
      case transform k of
        Nothing -> Left "could not transform head"
        Just v -> Right (v, r)

readBracketKey :: QueryKey -> QueryParser (T.Text, QueryKeyComponent)
readBracketKey (QueryKey query) = do
  case query of
    (b@(BracedValue val) : rest) -> pure (val, b)
    (b@EmptyBraces : rest) -> pure (mempty, b)
    _ -> failParse "no braced value to consume"

modifyTransformed :: (QueryKey -> Maybe QueryKey) -> QueryParser a -> QueryParser a
modifyTransformed tf (QueryParser cb) = QueryParser $ \transform key ->
  cb (transform >=> tf) key

droppingItem :: QueryKeyComponent -> QueryParser a -> QueryParser a
droppingItem item = modifyTransformed modify
  where
    modify (QueryKey (i : xs))
      | i == item = Just (QueryKey xs)
      | otherwise = Nothing
    modify _ = Nothing

droppingFirst :: QueryParser a -> QueryParser a
droppingFirst = modifyTransformed dropHead
  where
    dropHead (QueryKey (x : xs)) = Just (QueryKey xs)
    dropHead _ = Nothing

asTuple :: QueryParser a -> QueryParser (T.Text, a)
asTuple qp = do
  keyHead <- peekTransformedKeyHead
  (parsedText, item) <- readBracketKey keyHead
  (parsedText,) <$> droppingItem item qp

getTransform :: QueryParser (QueryKey -> Maybe QueryKey)
getTransform = QueryParser $ curry Right

failParse msg = QueryParser $ \_ _ -> Left msg

-- | Take the value at the head, ensuring along the way that the entire query matches.
takeValue :: QueryParser (Maybe BS.ByteString)
takeValue = QueryParser $ \transform queries ->
  case queries of
    [] -> Left "no values to take"
    ((k, v) : rest) ->
      case transform k of
        Nothing -> Left "bad transform"
        Just (QueryKey []) -> Right (v, rest)
        Just r -> Left $ "more query elements to consume: " <> show r

takeElement :: QueryParser BS.ByteString
takeElement = do
  r <- takeValue
  case r of
    Nothing -> failParse "No value"
    Just bs -> pure bs

takeNull :: QueryParser ()
takeNull = do
  r <- takeValue
  case r of
    Nothing -> pure ()
    Just x
      | x == "null" -> pure ()
      | otherwise -> failParse "encountered values where null was expected"

orParseError :: (Show err) => QueryParser (Either err a) -> QueryParser a
orParseError (QueryParser cb) = QueryParser $ \transform query ->
  case cb transform query of
    Left s -> Left s
    Right (r, q) -> case r of
      Left err -> Left $ show err
      Right a -> Right (a, q)

{-
manyEnding parser = many_v
  where
    many_v = some_v <|> (endpure []
    some_v = liftA2 (:) parser many_v
    -}

ensureConsumes :: QueryParser a -> QueryParser a
ensureConsumes a = QueryParser $ \transform q ->
  case runQueryParser a transform q of
    Left s -> Left s
    r@(Right (res, nq))
      | nq == q -> Left "did not consume input"
      | otherwise -> r

newtype JordanQueryParser a = JordanQueryParser {runJordanQueryParser :: QueryParser a}
  deriving (Functor, Applicative) via QueryParser
  deriving (Semigroup) via (Alt QueryParser a)

newtype JordanQueryObjectParser a = JordanQueryObjectParser {runJordanQueryObjectParser :: Permutation QueryParser a}
  deriving (Functor, Applicative) via (Permutation QueryParser)

addArrayBrackets :: QueryParser q -> QueryParser q
addArrayBrackets = modifyTransformed cb
  where
    cb (QueryKey (EmptyBraces : rest)) = Just (QueryKey rest)
    cb _ = Nothing

addJSONKey :: T.Text -> QueryParser q -> QueryParser q
addJSONKey k = modifyTransformed cb
  where
    cb (QueryKey (BracedValue val : rest))
      | val == k = Just (QueryKey rest)
      | otherwise = Nothing
    cb _ = Nothing

instance JSONObjectParser JordanQueryObjectParser where
  parseFieldWith field = \(JordanQueryParser q) ->
    JordanQueryObjectParser $
      asPermutation $
        addJSONKey field q
  parseFieldWithDefault f = \(JordanQueryParser q) def ->
    JordanQueryObjectParser $
      asPermutationWithDefault
        (addJSONKey f q)
        def

toBool :: BS.ByteString -> Either JSONError Bool
toBool "t" = pure True
toBool "true" = pure True
toBool "f" = pure False
toBool "false" = pure False
toBool _ = Left ErrorInvalidJSON

takeText :: QueryParser T.Text
takeText =
  orParseError $
    decodeUtf8' <$> takeElement

instance JSONTupleParser JordanQueryParser where
  consumeItemWith = \x -> x

instance JSONParser JordanQueryParser where
  parseTuple (JordanQueryParser p) =
    JordanQueryParser $
      addArrayBrackets p
  parseObject = \(JordanQueryObjectParser perm) ->
    JordanQueryParser $
      asParser perm
  parseArrayWith = \(JordanQueryParser parse) ->
    JordanQueryParser $ many $ ensureConsumes (addArrayBrackets parse)
  parseNull = JordanQueryParser takeNull
  parseText = JordanQueryParser takeText
  parseBool =
    JordanQueryParser $
      orParseError $ toBool <$> takeElement
  parseDictionary = \(JordanQueryParser parseValue) ->
    JordanQueryParser $ many $ ensureConsumes (asTuple parseValue)
  validateJSON (JordanQueryParser qp) =
    JordanQueryParser $
      orParseError qp
  parseNumber =
    JordanQueryParser $
      orParseError $
        first (const "not a valid number")
          . parseViaAttoparsec
          <$> takeElement

parseQueryToKeys :: Query -> [(QueryKey, Maybe BS.ByteString)]
parseQueryToKeys = mapMaybe transformElement
  where
    transformElement (k, v) = case AP.parseOnly parseQueryKey k of
      Left s -> Nothing
      Right qk -> Just (qk, v)

filterToStarting :: T.Text -> [(QueryKey, Maybe BS.ByteString)] -> [(QueryKey, Maybe BS.ByteString)]
filterToStarting key = mapMaybe keepElement
  where
    keepElement (k, v) =
      case k of
        (QueryKey (RawValue r : rest))
          | r == key -> Just (QueryKey rest, v)
          | otherwise -> Nothing
        _ -> Nothing

transformToKey key = filterToStarting key . parseQueryToKeys

parseQueryAtKeyWith :: (forall jsonParser. (JSONParser jsonParser) => jsonParser a) -> T.Text -> Query -> Either String a
parseQueryAtKeyWith (JordanQueryParser (QueryParser q)) key queryString = do
  fst <$> q Just (transformToKey key queryString)

hasQueryAtKey :: T.Text -> Query -> Bool
hasQueryAtKey k q = not (null $ transformToKey k q)

parseQueryAtKey :: (FromJSON a) => T.Text -> Query -> Either String a
parseQueryAtKey = parseQueryAtKeyWith fromJSON
