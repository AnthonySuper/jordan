{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant.OpenApi where

import Control.Lens
import Data.OpenApi.Declare
import Data.OpenApi.Internal
import Data.OpenApi.Lens
import Data.OpenApi.Operation
import Data.OpenApi.ParamSchema
import Data.OpenApi.Schema
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits
import Jordan
import Jordan.OpenAPI
import Jordan.Servant
import Jordan.Types.JSONError
import Network.HTTP.Media
import Servant.API
import Servant.API.ContentTypes
import Servant.API.Modifiers
import Servant.OpenApi.Internal

instance (ToJSON a, Typeable a) => ToSchema (ViaJordan a) where
  declareNamedSchema (Proxy :: Proxy (ViaJordan a)) = getToNamed (Proxy :: Proxy a)

instance forall a sub baseKey mods. (HasOpenApi sub, FromJSON a, KnownSymbol baseKey, SBoolI (FoldRequired mods)) => HasOpenApi (JordanQuery' baseKey mods a :> sub) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy sub)
      & addParam parameter
      & setResponseWith (<>) 400 queryErrorResponse
      & components . schemas %~ (<> defs)
    where
      (defs, ref) = runDeclare (getFromRef (Proxy @a)) mempty
      parameter :: Param
      parameter =
        mempty
          & name .~ T.pack (symbolVal $ Proxy @baseKey)
          & in_ .~ ParamQuery
          & style ?~ StyleDeepObject
          & explode ?~ True
          & allowReserved ?~ False
          & schema ?~ ref
          & required ?~ isRequired
      queryErrorResponse =
        pure $
          mempty & content . at ("application" // "json+haskell-jordan-query-error") . non mempty . schema ?~ Inline s
      s :: Schema
      s = toParamSchema (Proxy :: Proxy String)
      isRequired = case sbool @(FoldRequired mods) of
        STrue -> True
        SFalse -> False

errorResponse :: Declare (Definitions Schema) Response
errorResponse = do
  ref <- getToRef (Proxy :: Proxy JSONError)
  pure $ mempty & content . at ("application" // "json+haskell-servant-body-error") . non mempty . schema ?~ ref

instance forall a sub. (HasOpenApi sub, FromJSON a) => HasOpenApi (ReportingRequestBody a :> sub) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy sub)
      & addRequestBody reqBody
      & addErrorResponse
      & components . schemas %~ (<> defs)
    where
      (defs, ref) = runDeclare (getFromRef (Proxy :: Proxy a)) mempty
      reqBody :: RequestBody =
        mempty
          { _requestBodyContent = [("application" // "json", mediaType)]
          }
      mediaType :: MediaTypeObject =
        mempty
          { _mediaTypeObjectSchema = Just ref
          }
      addErrorResponse = setResponseWith (<>) 400 errorResponse
