{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Jordan.Servant
    where


import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Proxy (Proxy(..))
import Jordan
import Servant.API.ContentTypes

-- | Servant content type that lets you render or parse via Jordan.
--
-- Note: we will parse directly from a ByteString, or serialize directly to one.
data JordanJSON

instance Accept JordanJSON where
  contentType Proxy = contentType (Proxy :: Proxy JSON)
  contentTypes Proxy = contentTypes (Proxy :: Proxy JSON)

instance (ToJSON a) => MimeRender JordanJSON a where
  mimeRender Proxy = toJSONViaBuilder

instance (FromJSON a) => MimeUnrender JordanJSON a where
  mimeUnrender Proxy = parseOnly attoparsecParser
  mimeUnrenderWithType Proxy _ = parseOnly attoparsecParser
