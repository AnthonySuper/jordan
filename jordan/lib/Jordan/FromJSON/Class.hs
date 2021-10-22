{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
module Jordan.FromJSON.Class
    where

import Control.Applicative (Alternative(..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import Jordan.Generic.Options

-- | A class for parsing JSON objects.
class (Applicative f) => JSONObjectParser f where
  -- | Parse an object field with a given label, using a parser.
  --
  -- Note: in order to enable the generation of better documentation, use 'parseField' instead if at all possible!
  parseFieldWith
    ::  T.Text
    -- ^ Label of the field.
    -- Will be parsed into escaped text, if need be.
    -> (forall valueParser. JSONParser valueParser => valueParser a)
    -- ^ How to parse the field.
    -- Note the forall in this type signature: you cannot have this be specific to
    -- any particular implementation of parsing, to keep the parsing of a JSON abstract.
    -> f a
  parseField
    :: (FromJSON v)
    => T.Text
    -> f v
  parseField t = parseFieldWith t fromJSON

-- | A class for parsing JSON arrays.
class (Applicative f) => JSONTupleParser f where
  -- | Use a JSON parser to consume a single item of an array, then move onto the next one.
  --
  -- Note: you should prefer 'consumeItem' as it enables better documentation generation.
  consumeItemWith
    :: (forall valueParser. JSONParser valueParser => valueParser a)
    -> f a
  -- | Consume a single array item.
  consumeItem
    :: (FromJSON v)
    => f v
  consumeItem = consumeItemWith fromJSON

-- | Abstract class representing various parsers.
--
-- All parsers must have a Monoid instance that represents choice with failure as the identity.
class (Functor f, forall a. Monoid (f a)) => JSONParser f where
  parseObject
    :: T.Text
    -- ^ A label for the object.
    -- This label should, as much as possible, be "globally unique" in some way.
    -- This will enable better generation of documentation.
    -> (forall objectParser. JSONObjectParser objectParser => objectParser a)
    -- ^ Instructions on how to parse the object.
    -- Note that the actual implementation is kept abstract: you can only use methods found in JSONObjectParser, or
    -- combinators of those methods.
    -- This ensures that we can generate the proper parser in all cases.
    -> f a
  -- | Parse an object where you are okay if we parse strictly, IE, do not allow extra fields.
  -- This sometimes enables us to generate parsers that run faster.
  parseObjectStrict
    :: T.Text
    -> (forall objectParser. JSONObjectParser objectParser => objectParser a)
    -> f a
  parseObjectStrict = parseObject
  -- | Parse a dictionary of key-value pairs.
  parseDictionary
    :: (forall jsonParser. JSONParser jsonParser => jsonParser a)
    -> f [(T.Text, a)]

  -- | Parse a text field.
  parseText
    :: f T.Text
  parseTextConstant
    :: T.Text
    -> f ()
  parseTextConstant t = validateJSON (validated <$> parseText)
    where
      validated q
        | q == t = Right ()
        | otherwise = Left $ T.pack "Expected :" <> q
  -- | Use a tuple parser to parse an array.
  parseTuple
    :: (forall arrayParser. JSONTupleParser arrayParser => arrayParser o)
    -> f o
  parseArray
    :: (FromJSON a)
    => f [a]
  parseArray = parseArrayWith fromJSON
  parseArrayWith
    :: (forall jsonParser. JSONParser jsonParser => jsonParser a)
    -> f [a]
  parseNumber
    :: f Scientific
  parseNull
    :: f ()
  parseBool
    :: f Bool
  validateJSON
    :: f (Either T.Text a)
    -> f a

-- | A class to provide the canonical way to parse a JSON.
-- This class uses finally tagless tyle to keep the instructions for parsing abstract.
-- This allows us to automatically generate documentation, and to generate parsers that do not use intermediate structures.
--
-- This class is derivable generically, and will generate a \"nice\" format.
-- In my opinion, at least.
class FromJSON value where
  fromJSON :: (JSONParser f) => f value
  default fromJSON :: (Generic value, GFromJSON (Rep value)) => (JSONParser f => f value)
  fromJSON = to <$> gFromJSON @(Rep value) defaultOptions

instance FromJSON () where
  fromJSON = parseNull

instance {-# OVERLAPPABLE #-} (FromJSON a) => FromJSON [a] where
  fromJSON = parseArray

instance (FromJSON a) => FromJSON (Maybe a) where
  fromJSON = (Nothing <$ parseNull) <> (Just <$> fromJSON)

-- | Right-biased: will try to parse a 'Right' value first.
instance (FromJSON l, FromJSON r) => FromJSON (Either l r) where
  fromJSON = (Right <$> fromJSON) <> (Left <$> fromJSON)

instance (FromJSON Bool) where
  fromJSON = parseBool

instance FromJSON T.Text where
  fromJSON = parseText

instance FromJSON Int where
  fromJSON = fmap round parseNumber

instance FromJSON a => FromJSON (Map.Map T.Text a) where
  fromJSON = foldMap (uncurry Map.singleton) <$> parseDictionary fromJSON

data FromJSONOptions
  = FromJSONOptions
  { fromJSONEncodeSums :: SumTypeEncoding
  , fromJSONBaseName :: String
  }
  deriving (Show, Read, Eq, Ord, Generic)

defaultOptions :: FromJSONOptions
defaultOptions = FromJSONOptions TagInField ""

addName :: String -> FromJSONOptions -> FromJSONOptions
addName s d = d { fromJSONBaseName = fromJSONBaseName d <> s }

class GFromJSON v where
  gFromJSON :: (JSONParser f) => FromJSONOptions -> f (v a)

instance (FromJSON c) => GFromJSON (K1 i c) where
  gFromJSON _ = K1 <$> fromJSON

instance (GFromJSON f, Datatype t) => GFromJSON (D1 t f) where
  gFromJSON opts = M1 <$> gFromJSON (addName name opts)
    where
      name = moduleName s <> "." <> datatypeName s
      s :: D1 t f a
      s = undefined

instance {-# OVERLAPPABLE #-} forall c i. (GFromJSONObject i, Constructor c) => GFromJSON (C1 c i) where
  gFromJSON opts = M1 <$> parseObject (T.pack name) (gFromJSONObject opts)
    where
      name = fromJSONBaseName opts <> "." <> conName n
      n :: C1 c i a
      n = undefined

instance {-# OVERLAPS #-} (FromJSON s) => GFromJSON (C1 c (S1 (MetaSel 'Nothing su ss ds) (Rec0 s))) where
  gFromJSON _ = M1 . M1 . K1 <$> fromJSON

instance GFromJSON U1 where
  gFromJSON opts = U1 <$ parseNull

instance {-# OVERLAPS #-} (Constructor t) => GFromJSON (C1 t U1) where
  gFromJSON opts = M1 U1 <$ parseTextConstant conn
    where
      conn = T.pack $ conName c
      c :: C1 t U1 f
      c = undefined

instance {-# OVERLAPPABLE #-} (GFromJSON lhs, GFromJSON rhs) => GFromJSON (lhs :+: rhs) where
  gFromJSON opts
    = (L1 <$> gFromJSON opts)
    <> (R1 <$> gFromJSON opts)

instance {-# OVERLAPPABLE #-} forall t t' f f'. (GFromJSON (C1 t f), GFromJSON (C1 t' f'), Constructor t, Constructor t') => GFromJSON (C1 t f :+: C1 t' f') where
  gFromJSON opts =
    case fromJSONEncodeSums opts of
      TagVal -> (L1 <$> leftTagged) <> (R1 <$> rightTagged)
      TagInField -> (L1 <$> leftField) <> (R1 <$> rightField)
    where
      leftField :: (JSONParser p) => p (C1 t f a)
      leftField = parseObjectStrict (objName lhsName) $ parseFieldWith lhsName (gFromJSON opts)
      rightField :: (JSONParser p) => p (C1 t' f' a)
      rightField = parseObjectStrict (objName rhsName) $ parseFieldWith rhsName (gFromJSON opts)
      leftTagged
        = parseObject (objName lhsName)
        $ parseFieldWith "tag" (parseTextConstant lhsName)
        *> parseFieldWith "val" (gFromJSON opts)
      rightTagged
        = parseObject (objName rhsName)
        $ parseFieldWith "tag" (parseTextConstant rhsName)
        *> parseFieldWith "val" (gFromJSON opts)
      objName a = T.pack (fromJSONBaseName opts <> ".") <> a <> ".Input"
      lhsName = T.pack $ conName lhs
      lhs :: C1 t f a
      lhs = undefined
      rhsName = T.pack $ conName rhs
      rhs :: C1 t' f' a
      rhs = undefined

instance {-# OVERLAPPING #-} (Constructor t, Constructor t') =>
  GFromJSON (C1 t U1 :+: C1 t' U1) where
    gFromJSON ops = (L1 <$> gFromJSON ops) <> (R1 <$> gFromJSON ops)

class GFromJSONObject v where
  gFromJSONObject :: (JSONObjectParser f) => FromJSONOptions -> f (v a)

instance GFromJSONObject U1 where
  gFromJSONObject _ = pure U1

instance (FromJSON c, Selector t) => GFromJSONObject (S1 t (K1 v c)) where
  gFromJSONObject o
    = M1 . K1 <$> parseField (T.pack $ selName v)
      where
        v :: M1 S t f a
        v = undefined

instance (GFromJSONObject lhs, GFromJSONObject rhs) => GFromJSONObject (lhs :*: rhs) where
  gFromJSONObject o = (:*:) <$> gFromJSONObject o <*> gFromJSONObject o
