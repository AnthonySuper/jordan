{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Jordan.ToJSON.Builder
  ( JSONBuilder (..),
    toJSONViaBuilder,
    toJSONAsBuilder,
  )
where

import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Builder.Prim as BP
import Data.ByteString.Builder.Scientific (scientificBuilder)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (ord)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8BuilderEscaped)
import Data.Void (absurd)
import Data.Word (Word8)
import Jordan.ToJSON.Class

-- | JSON Serializer that makes use of 'Data.ByteString.Builder' to do its work.
-- Should be really fast.
newtype JSONBuilder a = JSONBuilder {runJSONBuilder :: a -> Builder}
  deriving (Semigroup, Monoid) via (a -> Builder)

instance Contravariant JSONBuilder where
  contramap f (JSONBuilder a) = JSONBuilder $ a . f

data CommaSep
  = Written !Builder
  | Empty

instance Semigroup CommaSep where
  Empty <> a = a
  a <> Empty = a
  (Written a) <> (Written b) = Written (a <> "," <> b)

instance Monoid CommaSep where
  mempty = Empty

runCommaSep :: CommaSep -> Builder
runCommaSep Empty = ""
runCommaSep (Written w) = w

newtype JSONCommaBuilder a = JSONCommaBuilder {runCommaBuilder :: a -> CommaSep}

-- Lifted from aeson (thanks to them!)
ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ const cs BP.>$< BP.char7 BP.>*< BP.char7
{-# INLINE ascii2 #-}

-- | Mostly lifted from Aeson (thanks to them).
escapeAscii :: BP.BoundedPrim Word8
escapeAscii =
  -- Irritatingly we have a few non-control characters we need to escape,
  -- so we try to do that first.
  BP.condB (== 0x5c) (ascii2 ('\\', '\\')) $ -- a backslash
    BP.condB (== 0x22) (ascii2 ('\\', '"')) $ -- a quote
      BP.condB (>= 0x20) (BP.liftFixedToBounded BP.word8) $ -- Now, if we have an ordinal above 0x20, we can just encode directly
        BP.condB (== 0x0a) (ascii2 ('\\', 'n')) $ -- Special control character \n
          BP.condB (== 0x0d) (ascii2 ('\\', 'r')) $ -- special control character \r
            BP.condB (== 0x09) (ascii2 ('\\', 't')) $ -- Special control character \t
              BP.condB (== 0x0c) (ascii2 ('\\', 'f')) $ -- Special control character \f
                BP.condB (== 0x08) (ascii2 ('\\', 'b')) $ -- Special control character \b
                  BP.liftFixedToBounded hexEscape -- fallback for other control characters
  where
    hexEscape :: BP.FixedPrim Word8
    hexEscape =
      (\c -> ('\\', ('u', ('0', ('0', c)))))
        BP.>$< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.char7 BP.>*< BP.word8HexFixed
{-# INLINE escapeAscii #-}

-- | Make a builder for a quoted string, which does all the cool escaping crap we need to do.
-- Mostly stolen shamelessly from Aeson.
serializeQuotedString :: Text -> Builder
serializeQuotedString t = "\"" <> encodeUtf8BuilderEscaped escapeAscii t <> "\""

serializeKV :: (a -> Builder) -> Text -> a -> Builder
serializeKV map k v = serializeQuotedString k <> ": " <> map v

instance Contravariant JSONCommaBuilder where
  contramap f (JSONCommaBuilder a) = JSONCommaBuilder $ a . f

instance Divisible JSONCommaBuilder where
  conquer = JSONCommaBuilder $ const Empty
  divide split sB sC = JSONCommaBuilder $ \a ->
    case split a of
      (b, c) -> runCommaBuilder sB b <> runCommaBuilder sC c

instance JSONObjectSerializer JSONCommaBuilder where
  serializeFieldWith field (JSONBuilder a) = JSONCommaBuilder $ \arg ->
    Written $ serializeQuotedString field <> ": " <> a arg
  serializeJust field (JSONBuilder a) = JSONCommaBuilder $ \case
    Nothing -> Empty
    Just a' -> Written $ serializeQuotedString field <> ": " <> a a'

instance JSONTupleSerializer JSONCommaBuilder where
  serializeItemWith (JSONBuilder a) = JSONCommaBuilder $ Written . a

instance Selectable JSONBuilder where
  giveUp f = JSONBuilder $ \a -> absurd (f a)
  select sel serL serR = JSONBuilder $ \a ->
    case sel a of
      Left lhs -> runJSONBuilder serL lhs
      Right rhs -> runJSONBuilder serR rhs

instance JSONSerializer JSONBuilder where
  serializeObject (JSONCommaBuilder f) = JSONBuilder $ \a ->
    case f a of
      Written bu -> "{" <> bu <> "}"
      Empty -> "{}"
  serializeTuple (JSONCommaBuilder f) = JSONBuilder $ \a ->
    case f a of
      Written bu -> "[" <> bu <> "]"
      Empty -> "[]"
  serializeDictionary (JSONBuilder t) = JSONBuilder $ \a ->
    "{" <> runCommaSep (foldMap (\(k, v) -> Written (serializeKV t k v)) a) <> "}"
  serializeText = JSONBuilder $ \t -> serializeQuotedString t
  serializeTextConstant = JSONBuilder . const . serializeQuotedString
  serializeNumber = JSONBuilder $ \a -> scientificBuilder a
  serializeNull = JSONBuilder $ const "null"
  serializeBool = JSONBuilder $ \case
    True -> "true"
    False -> "false"
  serializeArray = JSONBuilder $ \array ->
    case foldMap (Written . runJSONBuilder toJSON) array of
      Empty -> "[]"
      Written n -> "[" <> n <> "]"

-- | Serialize a Haskell datatype to a 'Builder'.
--
-- This is available for performance reasons: you may wish to use hPutBuilder
-- in order to (more or less) directly serialize some JSON object to a file handle.
toJSONAsBuilder :: (ToJSON a) => a -> Builder
toJSONAsBuilder = runJSONBuilder toJSON

-- | Serialize a Haskell datatype to a lazy ByteString.
toJSONViaBuilder :: (ToJSON a) => a -> LBS.ByteString
toJSONViaBuilder = toLazyByteString . runJSONBuilder toJSON
