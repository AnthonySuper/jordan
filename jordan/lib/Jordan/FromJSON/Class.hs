{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parse JSON using finally-tagless style.
--
-- This provides JSON parsing as an abstract interface.
-- This interface provides a way to parse JSON that is *inspectable*
-- and has some nice properties: for example, we can use it to build a parser that
-- directly parses your data structure, without building some intermediate value type!
module Jordan.FromJSON.Class where

import Control.Applicative (Alternative (..))
import Data.Coerce
import Data.Functor (($>))
import qualified Data.Int as I
import Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import Data.Proxy (Proxy (..))
import qualified Data.Ratio as Ratio
import Data.Scientific (Scientific)
import qualified Data.Semigroup as Semigroup
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Typeable
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Jordan.Generic.Options

-- | A class for parsing JSON objects.
class (Applicative f, Representational f) => JSONObjectParser f where
  -- | Parse an object field with a given label, using a parser.
  --
  -- Note: in order to enable the generation of better documentation, use 'parseField' instead if at all possible!
  parseFieldWith ::
    -- | Label of the field.
    -- Will be parsed into escaped text, if need be.
    T.Text ->
    -- | How to parse the field.
    -- Note the forall in this type signature: you cannot have this be specific to
    -- any particular implementation of parsing, to keep the parsing of a JSON abstract.
    (forall valueParser. JSONParser valueParser => valueParser a) ->
    f a

  parseDescribeFieldWith ::
    -- | Field key to parse
    T.Text ->
    -- | Description of the field
    T.Text ->
    -- | Parser for the field
    (forall valueParser. JSONParser valueParser => valueParser a) ->
    f a
  parseDescribeFieldWith field _ = parseFieldWith field
  parseField ::
    (FromJSON v) =>
    T.Text ->
    f v
  parseField t = parseFieldWith t fromJSON
  {-# INLINE parseField #-}
  parseDescribeField ::
    (FromJSON v) =>
    T.Text ->
    T.Text ->
    f v
  parseDescribeField key desc = parseDescribeFieldWith key desc fromJSON
  parseFieldWithDefault ::
    -- | Label of the field.
    T.Text ->
    -- | Parse the value from the field
    (forall valueParser. JSONParser valueParser => valueParser a) ->
    -- | Default value for the field
    a ->
    -- | Field in the object.
    f a
  parseDescribeFieldWithDefault ::
    -- | Label of the field
    T.Text ->
    -- | Description of the field
    T.Text ->
    -- | Parser for the field
    (forall valueParser. JSONParser valueParser => valueParser a) ->
    a ->
    f a
  parseDescribeFieldWithDefault field _ = parseFieldWithDefault field

-- | A class for parsing JSON arrays.
class (Applicative f, Representational f) => JSONTupleParser f where
  -- | Use a JSON parser to consume a single item of an array, then move onto the next one.
  --
  -- Note: you should prefer 'consumeItem' as it enables better documentation generation.
  consumeItemWith ::
    (forall valueParser. JSONParser valueParser => valueParser a) ->
    f a

  -- | Consume a single array item.
  consumeItem ::
    (FromJSON v) =>
    f v
  consumeItem = consumeItemWith fromJSON

-- | Abstract class representing various parsers.
--
-- All parsers must have a Monoid instance that represents choice with failure as the identity.
class (Functor f, forall a. Semigroup (f a), Representational f) => JSONParser f where
  parseObject ::
    -- | Instructions on how to parse the object.
    -- Note that the actual implementation is kept abstract: you can only use methods found in JSONObjectParser, or
    -- combinators of those methods.
    -- This ensures that we can generate the proper parser in all cases.
    (forall objectParser. JSONObjectParser objectParser => objectParser a) ->
    f a

  -- | Parse an object where you are okay if we parse strictly, IE, do not allow extra fields.
  -- This sometimes enables us to generate parsers that run faster.
  parseObjectStrict ::
    (forall objectParser. JSONObjectParser objectParser => objectParser a) ->
    f a
  parseObjectStrict = parseObject

  -- | Parse a dictionary of key-value pairs.
  parseDictionary ::
    (forall jsonParser. JSONParser jsonParser => jsonParser a) ->
    f [(T.Text, a)]

  -- | Parse a text field.
  parseText ::
    f T.Text

  parseTextConstant ::
    T.Text ->
    f ()
  parseTextConstant t = validateJSON (validated <$> parseText)
    where
      validated q
        | q == t = Right ()
        | otherwise = Left $ T.pack "Expected :" <> q

  -- | Use a tuple parser to parse an array.
  parseTuple ::
    (forall arrayParser. JSONTupleParser arrayParser => arrayParser o) ->
    f o

  parseArray ::
    (FromJSON a) =>
    f [a]
  parseArray = parseArrayWith fromJSON
  parseArrayWith ::
    (forall jsonParser. JSONParser jsonParser => jsonParser a) ->
    f [a]
  parseNumber ::
    f Scientific
  parseInteger ::
    f Integer
  parseInteger = round <$> parseNumber
  parseNull ::
    f ()
  parseBool ::
    f Bool
  validateJSON ::
    f (Either T.Text a) ->
    f a

  -- | Give a parser a unique name.
  -- May be used for documentation.
  nameParser ::
    T.Text ->
    f a ->
    f a
  nameParser _ a = a

  -- | Add information about the format of a particular parser.
  addFormat ::
    T.Text ->
    f a ->
    f a
  addFormat _ a = a

-- | A class to provide the canonical way to parse a JSON.
-- This class uses finally tagless tyle to keep the instructions for parsing abstract.
-- This allows us to automatically generate documentation, and to generate parsers that do not use intermediate structures.
--
-- This class is derivable generically, and will generate a \"nice\" format.
-- In my opinion, at least.
class FromJSON value where
  fromJSON :: (forall f. (JSONParser f, Representational f) => f value)
  {-# INLINE fromJSON #-}
  default fromJSON :: (Generic value, GFromJSON (Rep value), Typeable value) => (JSONParser f => f value)
  fromJSON = to <$> gFromJSON @(Rep value) defaultOptions {fromJSONBaseName = bn}
    where
      bn = T.unpack $ fullyQualifyName $ typeRep (Proxy :: Proxy value)

instance (Generic a, GFromJSON (Rep a), Typeable a, SpecifiesFromJSONOptions options) => FromJSON (WithOptions options a) where
  fromJSON = WithOptions . to <$> gFromJSON @(Rep a) (specifiedFromJSONOptions @options) {fromJSONBaseName = bn}
    where
      bn = T.unpack $ fullyQualifyName $ typeRep (Proxy :: Proxy a)

instance FromJSON () where
  fromJSON = parseNull

instance {-# OVERLAPPABLE #-} (FromJSON a) => FromJSON [a] where
  fromJSON = parseArray

instance {-# OVERLAPPING #-} FromJSON String where
  fromJSON = T.unpack <$> parseText

instance (FromJSON a) => FromJSON (Maybe a) where
  fromJSON = (Just <$> fromJSON) <> (parseNull $> Nothing)

-- | Right-biased: will try to parse a 'Right' value first.
instance (FromJSON l, FromJSON r) => FromJSON (Either l r) where
  fromJSON = (Right <$> fromJSON) <> (Left <$> fromJSON)

instance (FromJSON Bool) where
  fromJSON = parseBool

instance FromJSON T.Text where
  fromJSON = parseText

instance FromJSON Int where
  fromJSON = fromInteger <$> parseInteger

instance FromJSON Float where
  fromJSON = addFormat "float" $ realToFrac <$> parseNumber

instance FromJSON Double where
  fromJSON = addFormat "double" $ realToFrac <$> parseNumber

instance FromJSON I.Int32 where
  fromJSON = addFormat "int32" $ fromInteger <$> parseInteger

instance FromJSON I.Int64 where
  fromJSON = addFormat "int64" $ fromInteger <$> parseInteger

instance FromJSON Integer where
  fromJSON = parseInteger

instance FromJSON Scientific where
  fromJSON = parseNumber

instance forall a. (Integral a, FromJSON a) => FromJSON (Ratio.Ratio a) where
  fromJSON =
    parseObject $
      (Ratio.%)
        <$> parseDescribeField "num" "numerator of the ratio"
        <*> parseDescribeField "denom" "denominator of the ratio"

instance FromJSON a => FromJSON (Monoid.Dual a) where
  fromJSON = Monoid.Dual <$> fromJSON

instance FromJSON Monoid.All where
  fromJSON = Monoid.All <$> parseBool

instance FromJSON Monoid.Any where
  fromJSON = Monoid.Any <$> parseBool

instance FromJSON a => FromJSON (Monoid.Sum a) where
  fromJSON = Monoid.Sum <$> fromJSON

instance FromJSON a => FromJSON (Monoid.Product a) where
  fromJSON = Monoid.Product <$> fromJSON

instance FromJSON a => FromJSON (Monoid.First a) where
  fromJSON = Monoid.First <$> ((parseNull $> Nothing) <> (Just <$> fromJSON))

instance FromJSON a => FromJSON (Monoid.Last a) where
  fromJSON = Monoid.Last <$> ((parseNull $> Nothing) <> (Just <$> fromJSON))

instance FromJSON (f a) => FromJSON (Monoid.Alt f a) where
  fromJSON = Monoid.Alt <$> fromJSON

instance FromJSON (f a) => FromJSON (Monoid.Ap f a) where
  fromJSON = Monoid.Ap <$> fromJSON

instance FromJSON a => FromJSON (Semigroup.Min a) where
  fromJSON = Semigroup.Min <$> fromJSON

instance FromJSON a => FromJSON (Semigroup.Max a) where
  fromJSON = Semigroup.Max <$> fromJSON

instance FromJSON a => FromJSON (Semigroup.First a) where
  fromJSON = Semigroup.First <$> fromJSON

instance FromJSON a => FromJSON (Semigroup.Last a) where
  fromJSON = Semigroup.Last <$> fromJSON

-- containers package
instance (FromJSON a, Ord a) => FromJSON (Set.Set a) where
  fromJSON = Set.fromList <$> fromJSON

instance FromJSON a => FromJSON (Map.Map T.Text a) where
  fromJSON = foldMap (uncurry Map.singleton) <$> parseDictionary fromJSON

instance FromJSON a => FromJSON (Map.Map Integer a) where
  fromJSON = foldMap toSingleDict <$> parseDictionary fromJSON
    where
      toSingleDict (k, v) = case TR.signed TR.decimal k of
        Left s -> mempty
        Right (i, rest) -> if rest == mempty then Map.singleton i v else mempty

instance (FromJSON a) => FromJSON (NE.NonEmpty a) where
  fromJSON = validateJSON $ fmap toNonEmpty parseArray
    where
      toNonEmpty a = case NE.nonEmpty a of
        Nothing -> Left "Empty list"
        Just a -> pure a

data FromJSONOptions = FromJSONOptions
  { fromJSONEncodeSums :: SumTypeEncoding,
    fromJSONBaseName :: String,
    convertEnum :: String -> String,
    fromJSONOmitNothingFields :: Bool
  }
  deriving (Generic)

defaultOptions :: FromJSONOptions
defaultOptions = FromJSONOptions TagInField "" id True

class SpecifiesFromJSONOptions (a :: [*]) where
  specifiedFromJSONOptions :: FromJSONOptions

instance SpecifiesFromJSONOptions '[] where
  specifiedFromJSONOptions = defaultOptions

instance
  (SpecifiesFromJSONOptions xs) =>
  SpecifiesFromJSONOptions (OmitNothingFields ': xs)
  where
  specifiedFromJSONOptions = (specifiedFromJSONOptions @xs) {fromJSONOmitNothingFields = True}

instance
  (SpecifiesFromJSONOptions xs) =>
  SpecifiesFromJSONOptions (KeepNothingFields ': xs)
  where
  specifiedFromJSONOptions =
    (specifiedFromJSONOptions @xs) {fromJSONOmitNothingFields = False}

addName :: String -> FromJSONOptions -> FromJSONOptions
addName s d = d {fromJSONBaseName = fromJSONBaseName d <> s}

class GFromJSON v where
  gFromJSON :: (JSONParser f) => FromJSONOptions -> f (v a)

-- | Top-level metadata is ignored.
instance (FromJSON c) => GFromJSON (K1 i c) where
  gFromJSON _ = K1 <$> fromJSON

-- | Datatype metadata: we name the overall datatype with the baseName
-- provided in the options, then serialize the inner information.
instance (GFromJSON f, Datatype t) => GFromJSON (D1 t f) where
  gFromJSON opts = nameParser (T.pack (fromJSONBaseName opts)) $ M1 <$> gFromJSON opts

-- | If we have a constructor with arguments, and those arguments
-- do not have selectors (IE, this is not a record), then we should parse as a tuple.
instance
  {-# OVERLAPPABLE #-}
  (GFromJSONTuple inner, KnownSymbol n) =>
  GFromJSON (C1 (MetaCons n s 'False) inner)
  where
  gFromJSON opts = nameParser objName $ M1 <$> parseTuple (gFromJSONTuple opts)
    where
      objName = T.pack (fromJSONBaseName opts) <> "." <> conName
      conName = T.pack $ symbolVal (Proxy :: Proxy n)

-- | If we have a constructor with arguments, and those arguments
-- do have selectors (IE, this is a record), then we should parse as a record.
instance {-# OVERLAPS #-} forall c i n s. (GFromJSONObject i, KnownSymbol n) => GFromJSON (C1 (MetaCons n s 'True) i) where
  gFromJSON opts = M1 <$> nameParser (T.pack name) (parseObject $ gFromJSONObject opts)
    where
      name = fromJSONBaseName opts <> "." <> symbolVal (Proxy @n)

-- | Special-case: a one-argument constructor with no field selector gets its own parser, skipping the tuple entirely.
instance {-# OVERLAPS #-} (FromJSON inner, KnownSymbol n) => GFromJSON (C1 (MetaCons n s 'False) (S1 (MetaSel Nothing ss su dl) (Rec0 inner))) where
  gFromJSON opts =
    M1 . M1 . K1
      <$> fromJSON
    where
      connName = T.pack $ symbolVal $ Proxy @n

-- | When rendering a sum type, if we have a more complex value (IE, maybe
-- this is a constructor that takes arguments), we want to use whatever
-- sum encoding was provided in the options.
instance {-# OVERLAPPABLE #-} (GFromJSON (C1 t f), Constructor t) => GFromJSON (PartOfSum (C1 t f)) where
  gFromJSON opts = MkPartOfSum <$> encoded
    where
      encoded = case fromJSONEncodeSums opts of
        TagVal -> tagged
        TagInField -> field
      tagged =
        parseObject $
          parseFieldWith "tag" (parseTextConstant name)
            *> parseFieldWith "val" (gFromJSON opts)
      field =
        parseObject $
          parseFieldWith name (gFromJSON opts)
      name = T.pack $ conName (undefined :: C1 t f a)
      objName = T.pack (fromJSONBaseName opts <> ".") <> name

instance {-# OVERLAPPABLE #-} (KnownSymbol connName) => GFromJSON (C1 (MetaCons connName dontCare 'False) U1) where
  gFromJSON _ = M1 U1 <$ parseTextConstant constName
    where
      constName = T.pack (symbolVal $ Proxy @connName)

instance {-# OVERLAPS #-} (KnownSymbol connName) => GFromJSON (PartOfSum (C1 (MetaCons connName dontCare 'False) U1)) where
  gFromJSON opts = MkPartOfSum <$> gFromJSON opts

-- | If we can parse both sides of a sum-type, we can parse the entire sum type.
instance {-# OVERLAPS #-} (GFromJSON (PartOfSum l), GFromJSON (PartOfSum r)) => GFromJSON (l :+: r) where
  gFromJSON opts =
    (L1 . getPartOfSum <$> gFromJSON opts) <> (R1 . getPartOfSum <$> gFromJSON opts)

instance {-# OVERLAPS #-} (GFromJSON (PartOfSum l), GFromJSON (PartOfSum r)) => GFromJSON (PartOfSum (l :+: r)) where
  gFromJSON opts =
    MkPartOfSum
      <$> (L1 . getPartOfSum <$> gFromJSON opts) <> (R1 . getPartOfSum <$> gFromJSON opts)

-- | Class that helps us parse JSON objects.
class GFromJSONObject v where
  gFromJSONObject :: (JSONObjectParser f) => FromJSONOptions -> f (v a)

instance GFromJSONObject U1 where
  gFromJSONObject _ = pure U1

instance {-# OVERLAPPABLE #-} (FromJSON c, Selector t) => GFromJSONObject (S1 t (K1 v c)) where
  gFromJSONObject o =
    M1 . K1 <$> parseField (T.pack $ selName v)
    where
      v :: M1 S t f a
      v = undefined

instance {-# OVERLAPS #-} (FromJSON c, Selector t) => GFromJSONObject (S1 t (K1 v (Maybe c))) where
  gFromJSONObject o =
    M1 . K1 <$> parse
    where
      parse
        | fromJSONOmitNothingFields o = parseFieldWithDefault field ((Just <$> fromJSON) <> (parseNull $> Nothing)) Nothing
        | otherwise = parseField field
      field = T.pack $ selName v
      v :: M1 S t f a
      v = undefined

instance (GFromJSONObject lhs, GFromJSONObject rhs) => GFromJSONObject (lhs :*: rhs) where
  gFromJSONObject o = (:*:) <$> gFromJSONObject o <*> gFromJSONObject o

class GFromJSONTuple v where
  gFromJSONTuple :: (JSONTupleParser f) => FromJSONOptions -> f (v a)

instance (GFromJSONTuple lhs, GFromJSONTuple rhs) => GFromJSONTuple (lhs :*: rhs) where
  gFromJSONTuple o = (:*:) <$> gFromJSONTuple o <*> gFromJSONTuple o

instance (GFromJSON f) => GFromJSONTuple (S1 (MetaSel Nothing su ss ds) f) where
  gFromJSONTuple o = M1 <$> consumeItemWith (gFromJSON o)
