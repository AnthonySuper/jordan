{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.ToJSON.Class where

import Data.Foldable (fold)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Ratio as Ratio
import Data.Scientific
import qualified Data.Scientific as Sci
import qualified Data.Semigroup as Semi
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), TypeRep, Typeable, splitTyConApp, tyConModule, tyConName, typeRep, typeRepTyCon)
import Data.Void (Void, absurd)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
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

selected :: (Selectable f) => f lhs -> f rhs -> f (Either lhs rhs)
selected = select id

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
class (Divisible f, Representational f) => JSONObjectSerializer f where
  serializeFieldWith ::
    -- | Label for the field to serialize
    Text ->
    -- | How to serialize the field.
    -- The forall ensures that JSON serialization is kept completely abstract.
    -- You can only use the methods of 'JSONSerializer' here.
    (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a) ->
    f a
  serializeField :: (ToJSON a) => Text -> f a
  serializeField t = serializeFieldWith t toJSON
  serializeDescribeFieldWith ::
    -- | Field key to serialize.
    Text ->
    -- | Field description.
    Text ->
    -- | Serializer for the field.
    (forall valueSerializer. JSONSerializer valueSerializer => valueSerializer a) ->
    f a
  serializeDescribeFieldWith t _ = serializeFieldWith t

  -- | Write if we have Just a value. Do not add the field otherwise.
  serializeJust ::
    -- | Label for the field to serialize
    Text ->
    -- | Serializer for Just
    (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a) ->
    f (Maybe a)

class (Divisible f, Representational f) => JSONTupleSerializer f where
  serializeItemWith ::
    -- | Write a single item into the tuple.
    -- The forall keeps things abstract.
    (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a) ->
    f a
  serializeItem ::
    (ToJSON a) => f a
  serializeItem = serializeItemWith toJSON

-- | An abstract representation of how to serialize a Haskell value into JSON.
class (Selectable f, Representational f) => JSONSerializer f where
  serializeObject ::
    -- | How to serialize the object.
    -- The forall here keeps things abstract: you are only allowed to use the methods of 'JSONObjectSerializer' here.
    (forall objSerializer. JSONObjectSerializer objSerializer => objSerializer a) ->
    f a
  serializeDictionary ::
    (Foldable t) =>
    (forall jsonSerializer. JSONSerializer jsonSerializer => jsonSerializer a) ->
    f (t (Text, a))
  serializeText ::
    f Text

  -- | Serialize some text constant.
  -- Note that this returns a serializer of anything: if you are always going to serialize out the same string,
  -- we don't need to even look at the thing we\'re serializing!
  serializeTextConstant ::
    Text ->
    f a

  serializeNull ::
    f any
  serializeNumber ::
    f Scientific
  serializeBool ::
    f Bool
  serializeTuple ::
    (forall tupleSerializer. JSONTupleSerializer tupleSerializer => tupleSerializer a) ->
    f a
  serializeArray ::
    (ToJSON a) =>
    f [a]

  -- | Give a name to a serializer.
  -- Should be globally unique, if possible.
  nameSerializer ::
    Text ->
    f a ->
    f a
  nameSerializer _ a = a

-- | A class to provide the canonical way to encode a JSON.
--
-- This class uses finally tagless style to keep the instructions for serializing abstract.
-- This allows us to automatically generate documentation, and to generate serializers that always avoid the need for intermediate structures.
--
-- This class is derivable generically, and will generate a \"nice\" format.
-- In my opinion, at least.
--
-- If you want to customize this JSON, the newtype 'WithOptions' can be helpful, as it allows you to specify options for the generic serialization.
-- Unfortunately, due to a weird GHC quirk, you need to use it with @ -XStandaloneDeriving @ as well as @ -XDerivingVia @.
-- That is, you should write:
--
--
-- @
-- data PersonFilter = PersonFilter { filterFirstName :: Maybe Text, filterLastName :: Maybe Text }
--   deriving (Show, Read, Eq, Ord, Generic)
--
-- deriving via (WithOptions '[KeepNothingFields] PersonFilter) instance (ToJSON PersonFilter)
-- @

---- === __Laws__
--
-- This instance is lawless, unless 'Jordan.FromJSON.Class.FromJSON' is also defined for this type.
-- In that case, the representation serialized by 'ToJSON' should match that of the representation parsed by
-- 'Jordan.FromJSON.Class.FromJSON'.
class ToJSON v where
  toJSON :: (forall f. (JSONSerializer f) => f v)
  default toJSON :: (Generic v, GToJSON (Rep v), Typeable v) => (JSONSerializer f) => f v
  toJSON = contramap from $ gToJSON defaultToJSONOptions {toJSONBaseName = fq}
    where
      fq = T.unpack $ fullyQualifyName $ typeRep (Proxy :: Proxy v)

instance (Generic a, GToJSON (Rep a), Typeable a, SpecifiesToJSONOptions options) => ToJSON (WithOptions options a) where
  toJSON = contramap getWithOptions . contramap from $ gToJSON (specifiedToJSONOptions @options) {toJSONBaseName = fq}
    where
      fq = T.unpack $ fullyQualifyName $ typeRep (Proxy :: Proxy a)

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
  toJSON =
    serializeObject $
      divide divider (serializeField "num") (serializeField "denom")
    where
      divider :: Ratio.Ratio a -> (a, a)
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

instance (ToJSON a) => ToJSON (Map.Map Integer a) where
  toJSON = contramap (fmap toTextKey . Map.toList) $ serializeDictionary toJSON
    where
      toTextKey (key, value) = (pack $ show key, value)

instance (ToJSON a) => ToJSON (NE.NonEmpty a) where
  toJSON = contramap NE.toList serializeArray

instance (ToJSON a) => ToJSON (Set.Set a) where
  toJSON = contramap Set.toList serializeArray

data ToJSONOptions = ToJSONOptions
  { toJSONEncodeSums :: SumTypeEncoding,
    toJSONBaseName :: String,
    toJSONRenderEnum :: String -> String,
    toJSONOmitNothingFields :: Bool
  }

defaultToJSONOptions :: ToJSONOptions
defaultToJSONOptions =
  ToJSONOptions TagInField "" id True

class SpecifiesToJSONOptions (a :: [*]) where
  specifiedToJSONOptions :: ToJSONOptions

instance SpecifiesToJSONOptions '[] where
  specifiedToJSONOptions = defaultToJSONOptions

instance
  (SpecifiesToJSONOptions xs) =>
  SpecifiesToJSONOptions (OmitNothingFields ': xs)
  where
  specifiedToJSONOptions = (specifiedToJSONOptions @xs) {toJSONOmitNothingFields = True}

instance
  (SpecifiesToJSONOptions xs) =>
  SpecifiesToJSONOptions (KeepNothingFields ': xs)
  where
  specifiedToJSONOptions =
    (specifiedToJSONOptions @xs) {toJSONOmitNothingFields = False}

class GToJSON v where
  gToJSON :: (JSONSerializer s) => ToJSONOptions -> s (v a)

-- | Top-level metadata is ignored.
instance (ToJSON c) => GToJSON (K1 i c) where
  gToJSON _ = contramap (\(K1 a) -> a) toJSON

-- | Datatype metadata: we name the overall datatype with the baseName
-- passed in the options, then serialize the inner information.
instance (GToJSON f, Datatype t) => GToJSON (D1 t f) where
  gToJSON opts = nameSerializer (T.pack $ toJSONBaseName opts) $ contramap (\(M1 a) -> a) $ gToJSON opts

-- | Serialize out a no-argument constructor via a string value of its name.
-- This allows us to serialize out enum keys more easily.
--
-- This does not get a unique name as recursion cannot happen.
instance {-# OVERLAPS #-} (KnownSymbol name) => GToJSON (C1 (MetaCons name fixity 'False) U1) where
  gToJSON opts =
    serializeTextConstant (T.pack connNameS)
    where
      connNameS = symbolVal (Proxy :: Proxy name)

instance {-# OVERLAPS #-} (KnownSymbol name) => GToJSON (PartOfSum (C1 (MetaCons name fixity 'False) U1)) where
  gToJSON = contramap getPartOfSum . gToJSON

-- | IF we have a constructor with arguments, but not selectors, then
-- we serialize as a tuple.
instance {-# OVERLAPPABLE #-} (GToJSONTuple inner, Constructor (MetaCons n s 'False)) => GToJSON (C1 (MetaCons n s 'False) inner) where
  gToJSON opts =
    contramap (\(M1 a) -> a) $
      serializeTuple $ gToJSONTuple opts

-- | If we have a constructor with arguments AND selectors (IE, a record), then
-- we serialize out a JSON object.
instance {-# OVERLAPPABLE #-} (GToJSONObject inner, Constructor (MetaCons n s 'True)) => GToJSON (C1 (MetaCons n s 'True) inner) where
  gToJSON opts =
    contramap (\(M1 a) -> a) $
      serializeObject $
        gToJSONObject opts
    where
      name = T.pack $ toJSONBaseName opts <> "." <> conName (undefined :: C1 (MetaCons n s 'True) inner a)

-- | If we have a single-argument constructor with no selectors, we want to just parse it directly.
instance {-# OVERLAPS #-} (ToJSON i) => GToJSON (C1 (MetaCons n s 'False) (S1 (MetaSel 'Nothing su ss ds) (Rec0 i))) where
  gToJSON _ = contramap (\(M1 (M1 (K1 s))) -> s) toJSON

-- | When rendering a sum type, and this is NOT an enum value, render via
-- the sum encoding option the user provided.
instance {-# OVERLAPPABLE #-} (Constructor t, GToJSON (C1 t f)) => GToJSON (PartOfSum (C1 t f)) where
  gToJSON opts = contramap getPartOfSum encoded
    where
      encoded = case toJSONEncodeSums opts of
        TagVal -> tagged
        TagInField -> field
      field =
        serializeObject $
          serializeFieldWith cn (gToJSON opts)
      tagged =
        serializeObject $
          contramap ((),) $
            divided
              (serializeFieldWith "key" $ serializeTextConstant cn)
              (serializeFieldWith "value" $ gToJSON opts)
      objName = T.pack (toJSONBaseName opts) <> "." <> cn <> ".Output"
      cn = T.pack $ conName (undefined :: C1 t f a)

sumToEither :: (l :+: r) a -> Either (l a) (r a)
sumToEither f = case f of
  L1 a -> Left a
  R1 a -> Right a

-- | If we can serialize out both sides of a sum-type, we can serialize out the sum type.
instance forall l r. (GToJSON (PartOfSum l), GToJSON (PartOfSum r)) => GToJSON (l :+: r) where
  gToJSON :: forall f a. (JSONSerializer f) => ToJSONOptions -> f ((l :+: r) a)
  gToJSON opts =
    select
      sumToEither
      (contramap MkPartOfSum $ gToJSON opts)
      (contramap MkPartOfSum $ gToJSON opts)

instance (GToJSON (PartOfSum l), GToJSON (PartOfSum r)) => GToJSON (PartOfSum (l :+: r)) where
  gToJSON opts = contramap getPartOfSum (gToJSON opts)

instance GToJSON V1 where
  gToJSON _ = giveUp (error "how the hell did you construct a void data type?")

-- | Type class for generically converting to a JSON object.
-- We can do this if all the fields under a constructor are named.
class GToJSONObject v where
  gToJSONObject :: (JSONObjectSerializer f) => ToJSONOptions -> f (v a)

instance {-# OVERLAPPABLE #-} (GToJSON f, KnownSymbol selector) => GToJSONObject (S1 (MetaSel (Just selector) su ss ds) f) where
  gToJSONObject o =
    contramap (\(M1 a) -> a) $
      serializeFieldWith (T.pack $ symbolVal (Proxy :: Proxy selector)) (gToJSON o)

instance {-# OVERLAPS #-} (ToJSON a, KnownSymbol selector) => GToJSONObject (S1 (MetaSel (Just selector) su ss ds) (Rec0 (Maybe a))) where
  gToJSONObject o = contramap map fieldWriter
    where
      fieldWriter
        | toJSONOmitNothingFields o = serializeJust name toJSON
        | otherwise = serializeFieldWith name toJSON
      map (M1 (K1 a)) = a
      name = T.pack $ symbolVal (Proxy :: Proxy selector)

instance (GToJSONObject lhs, GToJSONObject rhs) => GToJSONObject (lhs :*: rhs) where
  gToJSONObject o = divide div (gToJSONObject o) (gToJSONObject o)
    where
      div (a :*: b) = (a, b)

class GToJSONTuple v where
  gToJSONTuple :: (JSONTupleSerializer f) => ToJSONOptions -> f (v a)

instance (GToJSONTuple lhs, GToJSONTuple rhs) => GToJSONTuple (lhs :*: rhs) where
  gToJSONTuple o = divide div (gToJSONTuple o) (gToJSONTuple o)
    where
      div (a :*: b) = (a, b)

instance (GToJSON f) => GToJSONTuple (S1 (MetaSel Nothing su ss ds) f) where
  gToJSONTuple o =
    contramap (\(M1 a) -> a) $
      serializeItemWith (gToJSON o)
