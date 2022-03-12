{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jordan.OpenAPI.Internal where

import Control.Applicative (Alternative (..))
import Control.Monad (unless, when)
import qualified Data.Aeson.Types as Aeson
import Data.Function (on)
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
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Debug.Trace (trace, traceM, traceShowId)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Jordan.FromJSON.Class
import Jordan.ToJSON.Class
import Network.HTTP.Types.URI (urlEncode)
import Optics.At.Core
import Optics.Iso (non)
import Optics.Operators
import Optics.Optic ((%), (&))

newtype ConstDeclare env r a = ConstDeclare {runConstDeclare :: Declare env r}

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

data PropertyDeclare = PropertyDeclare
  { requiredProperties :: [T.Text],
    propertyTypes :: InsOrd.InsOrdHashMap T.Text (Referenced Schema)
  }
  deriving (Show, Generic)

instance Semigroup PropertyDeclare where
  (PropertyDeclare r t) <> (PropertyDeclare r' t') =
    PropertyDeclare
      (r <> r')
      (t <> t')

instance Monoid PropertyDeclare where
  mempty = PropertyDeclare mempty mempty

newtype ObjectSchema a = ObjectSchema
  { getObjectSchema ::
      Declare (Definitions Schema) PropertyDeclare
  }
  deriving (Functor, Applicative, Contravariant, Divisible) via (ConstDeclare (Definitions Schema) PropertyDeclare)

addDescription ::
  T.Text ->
  Referenced Schema ->
  Referenced Schema
addDescription text = \case
  Ref ref ->
    Inline $
      #_schemaDescription ?~ text $
        #_schemaOneOf ?~ [Ref ref] $
          mempty
  Inline sc -> Inline $ case sc ^. #_schemaDescription of
    Nothing -> #_schemaDescription ?~ text $ sc
    Just txt ->
      #_schemaDescription ?~ text $
        #_schemaOneOf ?~ [Inline sc] $
          mempty

instance JSONObjectParser ObjectSchema where
  parseFieldWithDefault key = \prop _ -> ObjectSchema $ do
    r <- getRefDef $ getJSONSchema prop
    pure $ PropertyDeclare mempty $ InsOrd.singleton key r
  parseDescribeFieldWithDefault key description = \prop _ -> ObjectSchema $ do
    r <- getRefDef $ getJSONSchema prop
    pure $ PropertyDeclare mempty $ InsOrd.singleton key (addDescription description r)
  parseDescribeFieldWith key description = \prop -> ObjectSchema $ do
    r <- getRefDef $ getJSONSchema prop
    pure $ PropertyDeclare [key] $ InsOrd.singleton key (addDescription description r)
  parseFieldWith t p = ObjectSchema $ do
    r <- getRefDef (getJSONSchema p)
    pure $ PropertyDeclare [t] $ InsOrd.singleton t r

instance JSONObjectSerializer ObjectSchema where
  serializeFieldWith f w = ObjectSchema $ do
    r <- getRefDef (getJSONSchema w)
    pure $ PropertyDeclare [f] $ InsOrd.singleton f r
  serializeJust f w = ObjectSchema $ do
    r <- getRefDef (getJSONSchema w)
    pure $ PropertyDeclare [] $ InsOrd.singleton f r

newtype TupleSchema a = TupleSchema
  { getTupleSchema ::
      Declare (Definitions Schema) [Referenced Schema]
  }
  deriving (Functor, Applicative, Contravariant, Divisible) via (ConstDeclare (Definitions Schema) [Referenced Schema])

instance JSONTupleParser TupleSchema where
  consumeItemWith p = TupleSchema $ do
    r <- getRefDef $ getJSONSchema p
    pure [r]

newtype JSONSchema a = JSONSchema
  { getJSONSchema :: Declare (Definitions Schema) NamedSchema
  }
  deriving (Functor, Contravariant) via (ConstDeclare (Definitions Schema) NamedSchema)

instance Semigroup (JSONSchema a) where
  a <> b =
    JSONSchema . fmap unnamed $
      combineSchemas <$> getRefDef (getJSONSchema a) <*> getRefDef (getJSONSchema b)

instance Selectable JSONSchema where
  giveUp = mempty
  select _ (JSONSchema lhs) (JSONSchema rhs) = JSONSchema lhs <> JSONSchema rhs

sameTypes :: Schema -> Schema -> Bool
sameTypes = (==) `on` (^. #_schemaType)

bothHaveEnum a b = enumValues a /= [] && enumValues b /= []

enumValues = (^. #_schemaEnum % non [])

combineInline a b
  | sameTypes a b && bothHaveEnum a b = (#_schemaEnum ?~ (enumValues a <> enumValues b)) a
  | otherwise =
    (#_schemaOneOf ?~ fromMaybe [Inline a] (_schemaOneOf a) <> fromMaybe [Inline b] (_schemaOneOf b)) mempty

combineSchemas :: Referenced Schema -> Referenced Schema -> Schema
combineSchemas = curry $ \case
  (Inline a, Inline b) -> combineInline a b
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

getJSONRef ::
  forall a.
  FromJSON a =>
  Proxy a ->
  Declare (Definitions Schema) (Referenced Schema)
getJSONRef = getRefDef . getFromNamed

getRefDef ::
  Declare (Definitions Schema) NamedSchema ->
  Declare (Definitions Schema) (Referenced Schema)
getRefDef decl = case undeclare decl of
  NamedSchema (Just name) schema -> do
    known <- looks (InsOrd.member name)
    unless known $ do
      declare [(name, schema)]
      void decl
    return $ Ref (Reference name)
  _ -> Inline . (^. #_namedSchemaSchema) <$> decl

onlyUnnamed :: Schema -> JSONSchema a
onlyUnnamed a = JSONSchema $ pure $ NamedSchema Nothing a

encodeRefName :: T.Text -> T.Text
encodeRefName = decodeUtf8 . urlEncode False . encodeUtf8

instance JSONTupleSerializer TupleSchema where
  serializeItemWith s = TupleSchema $ do
    r <- getRefDef $ getJSONSchema s
    pure [r]

instance JSONParser JSONSchema where
  parseObject f = JSONSchema $ do
    d <- getObjectSchema f
    pure $
      NamedSchema Nothing $
        mempty
          & (#_schemaType ?~ OpenApiObject)
            . (#_schemaProperties .~ (d ^. #propertyTypes))
            . (#_schemaRequired .~ (d ^. #requiredProperties))
  parseDictionary inner = JSONSchema $ do
    r <- getRefDef (getJSONSchema inner)
    pure $
      unnamed $
        ( (#_schemaType ?~ OpenApiObject)
            . (#_schemaAdditionalProperties ?~ AdditionalPropertiesSchema r)
        )
          mempty
  parseTuple parser = JSONSchema $ do
    items <- getTupleSchema parser
    pure $
      mempty
        & ( unnamed
              . (#_schemaType ?~ OpenApiArray)
              . (#_schemaItems ?~ OpenApiItemsArray items)
          )
  parseArrayWith p = JSONSchema $ do
    itemRef <- getRefDef (getJSONSchema p)
    pure $
      ( unnamed
          . (#_schemaType ?~ OpenApiArray)
          . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      )
        mempty
  parseArray ::
    forall a.
    (FromJSON a) =>
    JSONSchema [a]
  parseArray = JSONSchema $ do
    itemRef <- getRefDef $ getFromNamed (Proxy :: Proxy a)
    pure $
      ( unnamed
          . (#_schemaType ?~ OpenApiArray)
          . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      )
        mempty
  parseNumber = onlyUnnamed $ (#_schemaType ?~ OpenApiNumber) mempty
  parseInteger = onlyUnnamed $ (#_schemaType ?~ OpenApiInteger) mempty
  parseTextConstant t =
    void
      . onlyUnnamed
      . (#_schemaType ?~ OpenApiString)
      . (#_schemaEnum ?~ [Aeson.String t])
      $ mempty
  parseNull = onlyUnnamed $ (#_schemaType ?~ OpenApiNull) mempty
  parseText = onlyUnnamed $ (#_schemaType ?~ OpenApiString) mempty
  parseBool = onlyUnnamed $ (#_schemaType ?~ OpenApiBoolean) mempty
  validateJSON (JSONSchema d) = JSONSchema d
  nameParser name = \schema -> JSONSchema $ do
    (NamedSchema _ schema) <- getJSONSchema schema
    pure $ NamedSchema (Just $ encodeRefName name) schema
  addFormat format =
    JSONSchema
      . fmap
        (#_namedSchemaSchema % #_schemaFormat ?~ format)
      . getJSONSchema

instance JSONSerializer JSONSchema where
  serializeObject f = JSONSchema $ do
    d <- getObjectSchema f
    pure $
      NamedSchema Nothing $
        mempty
          & (#_schemaType ?~ OpenApiObject)
            . (#_schemaProperties .~ (d ^. #propertyTypes))
            . (#_schemaRequired .~ (d ^. #requiredProperties))
  serializeTuple t = JSONSchema $ do
    items <- getTupleSchema t
    pure $
      ( unnamed
          . (#_schemaType ?~ OpenApiArray)
          . (#_schemaItems ?~ OpenApiItemsArray items)
      )
        mempty
  serializeArray :: forall a. (ToJSON a) => JSONSchema [a]
  serializeArray = JSONSchema $ do
    itemRef <- getRefDef $ getToNamed (Proxy :: Proxy a)
    pure $
      ( unnamed
          . (#_schemaType ?~ OpenApiArray)
          . (#_schemaItems ?~ OpenApiItemsObject itemRef)
      )
        mempty
  serializeText = parseText
  serializeBool = parseBool
  serializeNumber = parseNumber
  serializeDictionary ser = JSONSchema $ do
    r <- getRefDef (getJSONSchema ser)
    pure $
      unnamed $
        ( (#_schemaType ?~ OpenApiObject)
            . (#_schemaAdditionalProperties ?~ AdditionalPropertiesSchema r)
        )
          mempty
  serializeNull =
    case parseNull of
      JSONSchema a -> JSONSchema a
  serializeTextConstant t = let (JSONSchema a) = parseTextConstant t in JSONSchema a
  nameSerializer t = \ser -> JSONSchema $ do
    (NamedSchema _ s) <- getJSONSchema ser
    pure $ NamedSchema (Just $ encodeRefName t) s

-- | Get documentation for a type that implements FromJSON
getFromNamed :: forall a. (FromJSON a) => Proxy a -> Declare (Definitions Schema) NamedSchema
getFromNamed p = getJSONSchema (fromJSON :: JSONSchema a)

-- | Get a Referenced Schema for this schema member.
getFromRef :: forall a. (FromJSON a) => Proxy a -> Declare (Definitions Schema) (Referenced Schema)
getFromRef = getRefDef . getFromNamed

-- | Get documentation for a type that implements ToJSON
getToNamed :: forall a. (ToJSON a) => Proxy a -> Declare (Definitions Schema) NamedSchema
getToNamed p = getJSONSchema (toJSON :: JSONSchema a)

getToRef :: forall a. (ToJSON a) => Proxy a -> Declare (Definitions Schema) (Referenced Schema)
getToRef = getRefDef . getToNamed
