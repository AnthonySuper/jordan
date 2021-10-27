{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Jordan.ToJSON.Class
    where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.Map.Strict as Map
import qualified Data.Ratio as Ratio
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Semigroup as Semi
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Proxy(..), Typeable, tyConModule, tyConName, typeRep, typeRepTyCon)
import Data.Void (Void, absurd)
import GHC.Generics
import Jordan.Generic.Options

-- | Basically just 'Data.Functor.Contravariant.Divisible.Decidable' but without
-- a superclass constraint that we cannot implement for JSON.
--
-- More specifically, we can quite easily serialize some object into either a string or a number
-- as a top-level JSON value, but we cannot serialize both a string and a number as a top level key.
-- This means that we cannot implement 'Data.Functor.Contravariant.Divisible', but we can implement
-- all the operations from 'Data.Functor.Contravariant.Divisible.Decidable'.
--
-- This class lets us decide without being able to divide, which is fun to say.
class (Contravariant f) => Selectable f where
  -- | Give up trying to decide.
  giveUp :: (arg -> Void) -> f arg
  -- | Pick one thing, or another, as long as you can serialize both options.
  select :: (arg -> Either lhs rhs) -> f lhs -> f rhs -> f arg

-- | An abstract representation of how to serialize a JSON object.
-- Since serializing is the exact opposite of parsing, we have to be
-- 'Data.Functor.Contravariant.Decidable' instead of 'Control.Applicative.Alternative'.
--
-- That is, if we are serializing a JSON object, we need to be able to break things apart.
--
-- Unfortunately the combinators for breaking things apart are more annoying to use than
-- the combinators for putting things together, and involve a lot of tuples everywhere.
--
-- Thankfully we provide a good interface to derive these classes generically!
class (Divisible f) => JSONObjectSerializer f where
  writeField
    :: Text
    -- ^ Label for the field to write
    -> (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a)
    -- ^ How to write the field.
    -- The forall ensures that JSON serialization is kept completely abstract.
    -- You can only use the methods of 'JSONSerializer' here.
    -> f a

class (Divisible f) => JSONTupleSerializer f where
  writeItem
    :: (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a)
    -- ^ Write a single item into the tuple.
    -- The forall keeps things abstract.
    -> f a

-- | An abstract representation of how to serialize a Haskell value into JSON.
class (Selectable f) => JSONSerializer f where
  serializeObject
    :: Text
    -- ^ A name for the object. Should be "globally unique" as much as possible.
    -> (forall objSerializer. JSONObjectSerializer objSerializer => objSerializer a)
    -- ^ How to serialize the object.
    -- The forall here keeps things abstract: you are only allowed to use the methods of 'JSONObjectSerializer' here.
    -> f a
  serializeDictionary
    :: (Foldable t)
    => (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a)
    -> f (t (Text, a))
  serializeText
    :: f Text
  -- | Serialize some text constant.
  -- Note that this returns a serializer of anything: if you are always going to serialize out the same string,
  -- we don't need to even look at the thing we\'re serializing!
  serializeTextConstant
    :: Text
    -> f a
  serializeNull
    :: f any
  serializeNumber
    :: f Scientific
  serializeBool
    :: f Bool
  serializeTuple
    :: (forall tupleSerializer. JSONTupleSerializer tupleSerializer => tupleSerializer a)
    -> f a
  serializeArray
    :: (ToJSON a)
    => f [a]

-- | A class to provide the canonical way to encode a JSON.
--
-- This class uses finally tagless style to keep the instructions for serializing abstract.
-- This allows us to automatically generate documentation, and to generate serializers that always avoid the need for intermediate structures.
--
-- This class is derivable generically, and will generate a \"nice\" format.
-- In my opinion, at least.
class ToJSON v where
  toJSON :: (JSONSerializer f) => f v
  default toJSON :: (Generic v, GToJSON (Rep v)) => (JSONSerializer f) => f v
  toJSON = contramap from $ gToJSON defaultToJSONOptions

instance ToJSON () where
  toJSON = serializeNull

instance ToJSON Text where
  toJSON = serializeText

instance ToJSON Scientific where
  toJSON = serializeNumber

instance {-# OVERLAPPABLE #-} (ToJSON a) => ToJSON [a] where
  toJSON = serializeArray

-- | Nothings get serialized as null.
instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON = select find serializeNull toJSON
    where
      find Nothing = Left ()
      find (Just a) = Right a

instance (ToJSON lhs, ToJSON rhs) => ToJSON (Either lhs rhs) where
  toJSON = select id toJSON toJSON

instance ToJSON Bool where
  toJSON = serializeBool

instance ToJSON Int where
  toJSON = contramap fromIntegral serializeNumber

instance ToJSON Integer where
  toJSON = contramap fromInteger serializeNumber

instance ToJSON Float where
  toJSON = contramap realToFrac serializeNumber

instance ToJSON Double where
  toJSON = contramap realToFrac serializeNumber

instance {-# OVERLAPPING #-} ToJSON String where
  toJSON = contramap T.pack serializeText

instance forall a. (ToJSON a, Typeable a) => ToJSON (Ratio.Ratio a) where
  toJSON = serializeObject objName $
    divide divider (writeField "num" toJSON) (writeField "denom" toJSON)
    where
        divider :: Ratio.Ratio a -> (a,a)
        divider = (,) <$> Ratio.numerator <*> Ratio.denominator
        objName = T.pack $ tyName <> ".Ratio"
        tyName = (tyConModule <> const "." <> tyConName) $ typeRepTyCon $ typeRep (Proxy :: Proxy a)

instance (ToJSON a) => ToJSON (Semi.Min a) where
  toJSON = contramap Semi.getMin toJSON

instance (ToJSON a) => ToJSON (Semi.Max a) where
  toJSON = contramap Semi.getMax toJSON

instance (ToJSON a) => ToJSON (Semi.First a) where
  toJSON = contramap Semi.getFirst toJSON

instance (ToJSON a) => ToJSON (Semi.Last a) where
  toJSON = contramap Semi.getLast toJSON

instance (ToJSON a) => ToJSON (Semi.Dual a) where
  toJSON = contramap Semi.getDual toJSON

instance ToJSON Semi.All where
  toJSON = contramap Semi.getAll serializeBool

instance ToJSON Semi.Any where
  toJSON = contramap Semi.getAny serializeBool

instance (ToJSON a) => ToJSON (Semi.Sum a) where
  toJSON = contramap Semi.getSum toJSON

instance (ToJSON a) => ToJSON (Semi.Product a) where
  toJSON = contramap Semi.getProduct toJSON

instance (ToJSON a) => ToJSON (Map.Map Text a) where
  toJSON = contramap Map.toList $ serializeDictionary toJSON

data ToJSONOptions
  = ToJSONOptions
  { toJSONEncodeSums :: SumTypeEncoding
  , toJSONBaseName :: String
  , toJSONRenderEnum :: String -> String
  }

defaultToJSONOptions :: ToJSONOptions
defaultToJSONOptions
  = ToJSONOptions TagInField "" id

class GToJSON v where
  gToJSON :: (JSONSerializer s) => ToJSONOptions -> s (v a)

instance (ToJSON c) => GToJSON (K1 i c) where
  gToJSON _ = contramap (\(K1 a) -> a) toJSON

instance (GToJSON f, Datatype t) => GToJSON (D1 t f) where
  gToJSON = contramap (\(M1 a) -> a) . gToJSON . addName
    where
      addName b = b { toJSONBaseName = toJSONBaseName b <> dtname }
      dtname = moduleName s <> "." <> datatypeName s
      s :: D1 t f a
      s = undefined

instance {-# OVERLAPS #-} (Constructor t) => GToJSON (PartOfSum (C1 t U1)) where
  gToJSON opts = contramap getPartOfSum $ serializeTextConstant enumValue
    where
      enumValue = T.pack $ toJSONRenderEnum opts $ conName (undefined :: C1 t U1 f)

instance {-# OVERLAPPABLE #-} (Constructor t, GToJSON (C1 t f)) => GToJSON (PartOfSum (C1 t f)) where
  gToJSON opts = contramap getPartOfSum encoded
    where
      encoded = case toJSONEncodeSums opts of
        TagVal -> tagged
        TagInField -> field
      field = serializeObject objName $
        writeField cn (gToJSON opts)
      tagged = serializeObject objName $
        contramap ((),) $
          divided
            (writeField "key" $ serializeTextConstant cn)
            (writeField "value" $ gToJSON opts)
      objName = T.pack (toJSONBaseName opts) <> "." <> cn <> ".Output"
      cn =  T.pack $ conName (undefined :: C1 t f a)

sumToEither :: (l :+: r) a -> Either (l a) (r a)
sumToEither f = case f of
  L1 a -> Left a
  R1 a -> Right a

instance forall l r. (GToJSON (PartOfSum l), GToJSON (PartOfSum r)) => GToJSON (l :+: r) where
  gToJSON :: forall f a. (JSONSerializer f) => ToJSONOptions -> f ((l :+: r) a)
  gToJSON opts =
    select
      sumToEither
      (contramap PartOfSum $ gToJSON opts)
      (contramap PartOfSum $ gToJSON opts)

instance (GToJSON (PartOfSum l), GToJSON (PartOfSum r)) => GToJSON (PartOfSum (l :+: r)) where
  gToJSON opts = contramap getPartOfSum (gToJSON opts)

instance (GToJSON s) => GToJSON (S1 whatever s) where
  gToJSON = contramap (\(M1 a) -> a) . gToJSON

instance GToJSON V1 where
  gToJSON _ = giveUp (error "how the hell did you construct a void data type?")

class GToJSONObject v where
  gToJSONObject :: (JSONObjectSerializer f) => ToJSONOptions -> f (v a)

instance (GToJSON f, Selector t) => GToJSONObject (S1 t f) where
  gToJSONObject o
    = contramap (\(M1 a) -> a)
    $ writeField (T.pack $ selName v) (gToJSON o)
      where
        v :: M1 S t f a
        v = undefined

instance (GToJSONObject lhs, GToJSONObject rhs) => GToJSONObject (lhs :*: rhs) where
  gToJSONObject o = divide div (gToJSONObject o) (gToJSONObject o)
    where
      div (a :*: b) = (a,b)

instance {-# OVERLAPPABLE #-} (GToJSONObject inner, Constructor t) => GToJSON (C1 t inner) where
  gToJSON opts
    = contramap (\(M1 a) -> a)
    $ serializeObject name
    $ gToJSONObject opts
    where
      name = T.pack $ toJSONBaseName opts <> "." <> conName (undefined :: C1 t inner a) <> ".Output"

instance {-# OVERLAPS #-} (ToJSON i) => GToJSON (C1 c (S1 (MetaSel 'Nothing su ss ds) (Rec0 i))) where
  gToJSON _ = contramap (\(M1 (M1 (K1 s))) -> s) toJSON
