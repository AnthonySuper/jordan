{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant.Client
  ( HasClient (..),
    JordanQuery',
    ReportingRequestBody,
  )
where

import Data.Proxy
import Jordan
import Jordan.Servant
import Jordan.Servant.Client.Query
import Network.HTTP.Media
import Servant.API
import Servant.API.Modifiers
import Servant.Client.Core

-- | Note: This instance assumes that the 'Jordan.FromJSON' and 'Jordan.ToJSON' instances match.
-- This should be true for all types, ideally.
instance forall a m api. (ToJSON a, HasClient m api) => HasClient m (ReportingRequestBody a :> api) where
  type Client m (ReportingRequestBody a :> api) = a -> Client m api
  clientWithRoute pm Proxy req body =
    clientWithRoute pm (Proxy :: Proxy api) $ setRequestBodyLBS (toJSONViaBuilder body) ("application" // "json") req
  hoistClientMonad pm Proxy f cl = hoistClientMonad pm (Proxy @api) f . cl
