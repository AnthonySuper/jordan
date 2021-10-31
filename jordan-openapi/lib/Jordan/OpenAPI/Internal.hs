{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jordan.OpenAPI.Internal
    where

import Control.Applicative (Alternative(..))
import Control.Monad (unless)
import qualified Data.Aeson.Types as Aeson
import Data.Functor (void)
import Data.Functor.Const
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.Maybe (fromMaybe)
import Data.OpenApi.Declare
import Data.OpenApi.Internal
import Data.OpenApi.Internal.Schema (rename, unname, unnamed)
import Data.OpenApi.Optics
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Jordan.FromJSON.Class
import Jordan.ToJSON.Class
import Optics.At.Core
import Optics.Operators
import Optics.Optic ((%))

newtype ConstDeclare env r a
  = ConstDeclare { runConstDeclare :: Declare env r }

instance Functor (ConstDeclare env r) where
  fmap _ (ConstDeclare d) = ConstDeclare d

instance (Monoid r, Monoid env) => Applicative (ConstDeclare env r) where
  pure _ = ConstDeclare $ pure mempty
  (ConstDeclare f) <*> (ConstDeclare a) = ConstDeclare $ do
    f' <- f
    a' <- a
    pure $ f' <> a'

instance Contravariant (ConstDeclare env r) where
  contramap _ (ConstDeclare d) = ConstDeclare d

instance (Monoid r, Monoid env) => Divisible (ConstDeclare env r) where
  divide _ (ConstDeclare l) (ConstDeclare r) = ConstDeclare $ do
    l' <- l
    r' <- r
    pure $ l' <> r'
  conquer = ConstDeclare $ pure mempty

newtype ObjectSchema a
  = ObjectSchema
  { getObjectSchema
    :: Declare (Definitions Schema) Schema
  } deriving (Functor, Applicative, Contravariant, Divisible) via (ConstDeclare (Definitions Schema) Schema)

instance JSONObjectParser ObjectSchema where
  parseFieldWith t p = ObjectSchema $ do
    r <- getRefDef (getJSONSchema p)
    pure $ (#_schemaProperties % at t ?~ r) mempty
  parseField
    :: forall a. FromJSON a
    => T.Text
    -> ObjectSchema a
  parseField label = ObjectSchema $ do
    ref <- getJSONRef (Proxy :: Proxy a)
    pure $ (#_schemaProperties % at label ?~ ref) mempty

instance JSONObjectSerializer ObjectSchema where
  writeField f w = ObjectSchema $ do
    r <- getRefDef (getJSONSchema w)
    pure $ (#_schemaProperties % at f ?~ r) mempty

newtype TupleSchema a
  = TupleSchema
    { getTupleSchema
        :: Declare (Definitions Schema) [Referenced Schema]
    } deriving (Functor, Applicative, Contravariant, Divisible) via (ConstDeclare (Definitions Schema) [Referenced Schema])

instance JSONTupleParser TupleSchema where
  consumeItemWith p = TupleSchema $ do
    r <- getRefDef $ getJSONSchema p
    pure [r]

newtype JSONSchema a
  = JSONSchema
  { getJSONSchema :: Declare (Definitions Schema) NamedSchema
  } deriving (Functor, Contravariant) via (ConstDeclare (Definitions Schema) NamedSchema)

instance Semigroup (JSONSchema a) where
  a <> b = JSONSchema . fmap unnamed $
    combineSchemas <$> getRefDef (getJSONSchema a) <*> getRefDef (getJSONSchema b)

instance Selectable JSONSchema where
  giveUp = mempty
  select _ (JSONSchema lhs) (JSONSchema rhs) = JSONSchema lhs <> JSONSchema rhs

combineSchemas :: Referenced Schema -> Referenced Schema -> Schema
combineSchemas = curry $ \case
  (Inline a, Inline b) ->
    (#_schemaOneOf ?~ fromMaybe [Inline a] (_schemaOneOf a) <> fromMaybe [Inline b] (_schemaOneOf b)) mempty
  (Inline a, Ref b) ->
    (#_schemaOneOf ?~ fromMaybe [Inline a] (_schemaOneOf a) <> [Ref b]) mempty
  (Ref a, Inline b) ->
    (#_schemaOneOf ?~ [Ref a] <> fromMaybe [Inline b] (_schemaOneOf b)) mempty
  (Ref a, Ref b) -> (#_schemaOneOf ?~ [Ref a, Ref b]) mempty

-- | Empty instance: must be both a boolean and a text value, which is not possible (obviously!)
instance Monoid (JSONSchema a) where
  mempty = JSONSchema $ do
    t <- getJSONSchema parseText
    b <- getJSONSchema parseBool
    pure $ unnamed $ (#_schemaAllOf ?~ [Inline $ _namedSchemaSchema t, Inline $ _namedSchemaSchema b]) mempty

getJSONRef
  :: forall a. FromJSON a
  => Proxy a
  -> Declare (Definitions Schema) (Referenced Schema)
getJSONRef = getRefDef . getFromNamed

getRefDef
  :: Declare (Definitions Schema) NamedSchema
  -> Declare (Definitions Schema) (Referenced Schema)
getRefDef decl = case undeclare decl of
    NamedSchema (Just name) schema -> do
      known <- looks (InsOrd.member name)
      unless known $ do
        declare [(name, schema)]
        void decl
      return $ Ref (Reference name)
    _ -> Inline . _namedSchemaSchema <$> decl

onlyUnnamed :: Schema -> JSONSchema a
onlyUnnamed a = JSONSchema $ pure $ NamedSchema Nothing a

instance JSONTupleSerializer TupleSchema where
  writeItem s = TupleSchema $ do
    r <- getRefDef $ getJSONSchema s
    pure [r]

instance JSONParser JSONSchema where
  parseObject n f = JSONSchema $ do
    d <- getObjectSchema f
    pure $ NamedSchema (Just n) d
  parseDictionary inner = JSONSchema $ do
    r <- getRefDef (getJSONSchema inner)
    pure $ unnamed $
      ( (#_schemaType ?~ OpenApiObject)
      . (#_schemaAdditionalProperties ?~ AdditionalPropertiesSchema r)
      ) mempty
  parseTuple parser = JSONSchema $ do
    items <- getTupleSchema parser
    pure $
      ( unnamed
      . (#_schemaType ?~ OpenApiArray)
      . (#_schemaItems ?~ OpenApiItemsArray items)
      ) mempty
  parseArrayWith p = JSONSchema $ do
    itemRef <- getRefDef (getJSONSchema p)
    pure $
      ( unnamed
      . (#_schemaType ?~ OpenApiArray)
      . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      ) mempty
  parseArray
    :: forall a. (FromJSON a)
    => JSONSchema [a]
  parseArray = JSONSchema $ do
    itemRef <- getRefDef $ getFromNamed (Proxy :: Proxy a)
    pure $
      ( unnamed
      . (#_schemaType ?~ OpenApiArray)
      . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      ) mempty
  parseNumber = onlyUnnamed $ (#_schemaType ?~ OpenApiNumber) mempty
  parseTextConstant t
    = void
    . onlyUnnamed
    . (#_schemaType ?~ OpenApiString)
    . (#_schemaEnum ?~ [Aeson.String t])
    $ mempty
  parseNull = onlyUnnamed $ (#_schemaType ?~ OpenApiNull) mempty
  parseText = onlyUnnamed $  (#_schemaType ?~ OpenApiString) mempty
  parseBool = onlyUnnamed $ (#_schemaType ?~ OpenApiBoolean) mempty
  validateJSON (JSONSchema d) = JSONSchema d

instance JSONSerializer JSONSchema where
  serializeObject t f = JSONSchema $ do
    d <- getObjectSchema f
    pure $ NamedSchema (Just t) d
  serializeTuple t = JSONSchema $ do
    items <- getTupleSchema t
    pure $
      ( unnamed
      . (#_schemaType ?~ OpenApiArray)
      . (#_schemaItems ?~ OpenApiItemsArray items)
      ) mempty
  serializeArray :: forall a. (ToJSON a) => JSONSchema [a]
  serializeArray = JSONSchema $ do
    itemRef <- getRefDef $ getToNamed (Proxy :: Proxy a)
    pure $
      ( unnamed
      . (#_schemaType ?~ OpenApiArray)
      . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      ) mempty
  serializeText = parseText
  serializeBool = parseBool
  serializeNumber = parseNumber
  serializeDictionary ser = JSONSchema $ do
    r <- getRefDef (getJSONSchema ser)
    pure $ unnamed $
      ( (#_schemaType ?~ OpenApiObject)
      . (#_schemaAdditionalProperties ?~ AdditionalPropertiesSchema r)
      ) mempty
  serializeNull =
    case parseNull of
      JSONSchema a -> JSONSchema a
  serializeTextConstant t = let (JSONSchema a) = parseTextConstant t in JSONSchema a

-- | Get documentation for a type that implements FromJSON
getFromNamed :: forall a. (FromJSON a) => Proxy a -> Declare (Definitions Schema) NamedSchema
getFromNamed p = getJSONSchema (fromJSON :: JSONSchema a)

-- | Get documentation for a type that implements ToJSON
getToNamed :: forall a. (ToJSON a) => Proxy a -> Declare (Definitions Schema) NamedSchema
getToNamed p = getJSONSchema (toJSON :: JSONSchema a)
