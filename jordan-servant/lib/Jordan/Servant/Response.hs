{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Helpers for rendering responses via Jordan.
module Jordan.Servant.Response where

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Proxy
import GHC.Generics
import Jordan
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Modifiers

-- | Wrapper to perform JSON serialization via Jordan.
--
-- Types used with this wrapper should have isomorphic 'Jordan.ToJSON' and 'Jordan.FromJSON' instances.
-- A utility is provided to check this.
newtype ViaJordan a = ViaJordan {getViaJordan :: a}
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON a => FromJSON (ViaJordan a) where
  fromJSON = ViaJordan <$> fromJSON

instance ToJSON a => ToJSON (ViaJordan a) where
  toJSON = contramap getViaJordan toJSON

instance (HasStatus a) => HasStatus (ViaJordan a) where
  type StatusOf (ViaJordan a) = StatusOf a

-- | Overlapping instance: sidestep Aeson, use Jordan.
instance {-# OVERLAPPING #-} (ToJSON a) => MimeRender JSON (ViaJordan a) where
  mimeRender Proxy = toJSONViaBuilder . getViaJordan

-- | Overlapping instance: sidestep Aeson, just Jordan.
instance {-# OVERLAPPING #-} (FromJSON a) => MimeUnrender JSON (ViaJordan a) where
  mimeUnrender Proxy = parseOnly (ViaJordan <$> attoparsecParser)
  mimeUnrenderWithType Proxy _ = parseOnly (ViaJordan <$> attoparsecParser)
