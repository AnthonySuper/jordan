{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant
  ( JordanJSON,
    ReportingRequestBody,
    JordanQuery',
    RequiredJordanQuery,
    OptionalJordanQuery,
    ViaJordan (..),
  )
where

import Data.Attoparsec.ByteString.Lazy (parseOnly)
import Data.Proxy (Proxy (..))
import Data.Typeable (Proxy (..))
import Jordan.FromJSON.Attoparsec (attoparsecParser)
import Jordan.FromJSON.Class (FromJSON (..))
import Jordan.FromJSON.UnboxedReporting (parseOrReport)
import Jordan.Servant.Query
  ( JordanQuery',
    OptionalJordanQuery,
    RequiredJordanQuery,
  )
import Jordan.Servant.Response (ViaJordan (..))
import Jordan.ToJSON.Builder (toJSONViaBuilder)
import Jordan.ToJSON.Class (ToJSON (..))
import Jordan.Types.JSONError (JSONError)
import Network.HTTP.Media (matchContent)
import Network.HTTP.Media.MediaType ((//), (/:))
import Network.HTTP.Types.Header (hContentType)
import Servant.API
  ( Accept (..),
    HasLink (..),
    MimeRender (..),
    MimeUnrender (..),
    type (:>),
  )

-- | Servant content type that lets you render or parse via Jordan.
--
-- Note: It is generally better to use 'ViaJordan' instead, which gets you nice API documentation.
-- However, you might want to slowly migrate your API to Jordan.
-- In this case, you can use this as a content type.
data JordanJSON

-- | Jordan JSON will have a content type of @ application/json; haskell-encoder=jordan; encoding=utf-8 @.
-- This allows you to conditionally request the Jordanified response.
instance Accept JordanJSON where
  contentType Proxy = "application" // "json" /: ("haskell-encoder", "jordan") /: ("encoding", "utf-8")
  contentTypes p@Proxy =
    pure (contentType p)
      <> pure (("application" // "json") /: ("encoding", "utf-8"))
      <> pure ("application" // "json")

-- | Uses 'Jordan.toJSONViaBuilder'
instance (ToJSON a) => MimeRender JordanJSON a where
  mimeRender Proxy = toJSONViaBuilder

-- | Parses directly from a lazy bytestring via Attoparsec
instance (FromJSON a) => MimeUnrender JordanJSON a where
  mimeUnrender Proxy = parseOnly attoparsecParser
  mimeUnrenderWithType Proxy _ = parseOnly attoparsecParser

-- | A parameter for use with Servant, which lets you parse the request body or report parse errors to the user.
-- It is different from using the existing ReqBody param from Servant in that it will give a detailed report of why the format of the request
-- body was wrong if need be.
--
-- This will use 'Jordan.parseJSONReporting' for its work.
-- This is generally a little slower than direct attoparsec parsing, but avoids us having to parse twice.
data ReportingRequestBody a

instance HasLink sub => HasLink (ReportingRequestBody a :> sub) where
  type MkLink (ReportingRequestBody a :> sub) r = MkLink sub r
  toLink toA (Proxy :: Proxy (ReportingRequestBody a :> sub)) = toLink toA (Proxy @sub)
