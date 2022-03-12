{-# LANGUAGE ScopedTypeVariables #-}

module Jordan.OpenAPI
  ( getFromNamed,
    getToNamed,
    getFromRef,
    getToRef,
    JordanFromJSONSchema (..),
    JordanToJSONSchema (..),
  )
where

import Data.Functor.Contravariant (contramap)
import Data.OpenApi.Schema (ToSchema (..))
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable (..))
import Jordan.FromJSON.Class (FromJSON (..))
import Jordan.OpenAPI.Internal (getFromNamed, getFromRef, getToNamed, getToRef)
import Jordan.ToJSON.Class (ToJSON (..))

-- | Newtype for use with DerivingVia.
--
-- Allows deriving 'Data.OpenApi.Schema.ToSchema' via DerivingVia, using the Jordan
-- defintion of 'Jordan.ToJSON.Class.ToJSON'.
newtype JordanFromJSONSchema a = JordanFromJSONSchema {getJordanFromJSONSchema :: a}

instance (FromJSON a) => FromJSON (JordanFromJSONSchema a) where
  fromJSON = JordanFromJSONSchema <$> fromJSON

instance (Typeable a, FromJSON a) => ToSchema (JordanFromJSONSchema a) where
  declareNamedSchema (Proxy :: Proxy (JordanFromJSONSchema a)) = getFromNamed (Proxy :: Proxy a)

-- | Newtype for use with DerivingVia.
--
-- Allows deriving 'Data.OpenApi.Schema.ToSchema' via DerivingVia, using the Jordan
-- defintion of 'Jordan.ToJSON.Class.ToJSON'.
newtype JordanToJSONSchema a = JordanToJSONSchema {getJordanToJSONSchema :: a}

instance (ToJSON a) => ToJSON (JordanToJSONSchema a) where
  toJSON = contramap getJordanToJSONSchema toJSON

instance (Typeable a, ToJSON a) => ToSchema (JordanToJSONSchema a) where
  declareNamedSchema (Proxy :: Proxy (JordanToJSONSchema a)) = getToNamed (Proxy :: Proxy a)
