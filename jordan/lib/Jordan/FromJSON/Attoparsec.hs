{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of FromJSON parsers via Attoparsec.
--
-- This module does not construct intermediate data structures like maps or key-value lists,
-- and instead uses permutation parsers in order to parse your data structure directly.
-- This means that it is pretty fast!
-- However, you also get basically *zero* error reporting, which is generally not what you want.
module Jordan.FromJSON.Attoparsec
  ( attoparsecParserFor,
    parseViaAttoparsecWith,
    parseViaAttoparsec,
    attoparsecParser,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (void, when)
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as CH
import qualified Data.Attoparsec.Combinator as AC
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Char (chr, digitToInt, isControl, isHexDigit, ord)
import Data.Functor (void, ($>))
import Data.Monoid (Alt (..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Debug.Trace
import Jordan.FromJSON.Class
import Jordan.FromJSON.Internal.Attoparsec
import Jordan.FromJSON.Internal.Permutation
import Numeric (showHex)
import qualified Text.Megaparsec as Text

newtype ObjectParser a = ObjectParser
  {runObjectParser :: Permutation AP.Parser a}
  deriving (Functor, Applicative)

type role ArrayParser representational

data ArrayParser a
  = ParseNoEffect a
  | ParseWithEffect (AP.Parser a)
  deriving (Functor)

instance Applicative ArrayParser where
  pure = ParseNoEffect
  f <*> a = case f of
    ParseNoEffect fab -> case a of
      ParseNoEffect a' -> ParseNoEffect (fab a')
      ParseWithEffect pa -> ParseWithEffect (fab <$> pa)
    ParseWithEffect pa -> case a of
      ParseNoEffect a' -> ParseWithEffect (fmap ($ a') pa)
      ParseWithEffect pa' -> ParseWithEffect $ do
        f' <- pa
        comma
        f' <$> pa'

runArrayParser :: ArrayParser a -> AP.Parser a
runArrayParser (ParseNoEffect a) = pure a
runArrayParser (ParseWithEffect eff) = eff

instance JSONObjectParser ObjectParser where
  parseFieldWith label parser =
    ObjectParser $
      asPermutation $
        parseObjectField
          label
          (runAttoparsecParser parser)
  {-# INLINE parseFieldWith #-}
  parseFieldWithDefault f = \(AttoparsecParser parseField) def ->
    ObjectParser $
      asPermutationWithDefault (parseObjectField f (parseField <?> ("field " <> show f))) def

newtype AttoparsecParser a = AttoparsecParser
  {runAttoparsecParser :: AP.Parser a}
  deriving (Functor)
  deriving (Semigroup, Monoid) via (Alt AP.Parser a)

instance JSONTupleParser ArrayParser where
  consumeItemWith = ParseWithEffect . runAttoparsecParser

instance JSONParser AttoparsecParser where
  parseObject = \parser -> AttoparsecParser $
    label "Object" $ do
      startObject
      r <- wrapEffect (parseAnyField <?> "ignored field in the middle of an object") comma $ runObjectParser parser
      objectEndWithJunk
      pure r
  {-# INLINE parseObject #-}
  parseDictionary parse = AttoparsecParser $
    inObjectBraces $ do
      parseDictField (runAttoparsecParser parse) `AP.sepBy` comma
  parseTextConstant c = AttoparsecParser (objectKey c <?> "text constant" <> Text.unpack c)
  {-# INLINE parseTextConstant #-}
  parseText = AttoparsecParser parseJSONText
  {-# INLINE parseText #-}
  parseNumber = AttoparsecParser number
  {-# INLINE parseNumber #-}
  validateJSON v = AttoparsecParser $ do
    r <- runAttoparsecParser v
    case r of
      Left err -> fail (Text.unpack err)
      Right e -> pure e
  parseTuple ap = AttoparsecParser $ do
    lexeme $ AP.word8 91
    r <- runArrayParser ap
    lexeme $ AP.word8 93
    pure r
  {-# INLINE parseTuple #-}
  parseArrayWith jp = AttoparsecParser $ do
    startArray
    r <- lexeme (runAttoparsecParser jp) `AP.sepBy` comma <?> "array items"
    endArray
    pure r
  {-# INLINE parseArrayWith #-}
  parseBool =
    AttoparsecParser $
      lexeme $
        (AP.string "true" $> True) <|> (AP.string "false" $> False)
  parseNull = AttoparsecParser $ lexeme (AP.string "null" $> ())
  nameParser l = \(AttoparsecParser a) ->
    AttoparsecParser $
      label ("Parser '" <> Text.unpack l <> "'") a

-- | Convert an abstract JSON parser to an Attoparsec Parser.
-- This function will skip leading whitespace.
attoparsecParserFor :: (forall parser. JSONParser parser => parser a) -> AP.Parser a
attoparsecParserFor = (skipSpace *>) . runAttoparsecParser
{-# INLINE attoparsecParserFor #-}

parseViaAttoparsecWith :: (forall parser. JSONParser parser => parser a) -> ByteString -> Either String a
parseViaAttoparsecWith p = AP.parseOnly (attoparsecParserFor p)
{-# INLINE parseViaAttoparsecWith #-}

-- | Parse a ByteString via an Attoparsec Parser.
parseViaAttoparsec :: forall val. (FromJSON val) => ByteString -> Either String val
parseViaAttoparsec = parseViaAttoparsecWith (fromJSON @val)
{-# INLINE parseViaAttoparsec #-}

-- | Get an Attoparsec parser for a particular JSON-parsable value.
attoparsecParser :: (FromJSON val) => AP.Parser val
attoparsecParser = runAttoparsecParser fromJSON
{-# INLINE attoparsecParser #-}
