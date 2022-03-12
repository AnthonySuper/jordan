{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides instances from server-related types to jordan API combinators.
--
-- Sadly, this does mean that this module has a bunch of orphan instances.
-- Please, *please* use these instead of any others.
-- Please.
module Jordan.Servant.Server where

import Control.Monad.IO.Class
import Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits
import Jordan
import Jordan.Servant
import Jordan.Servant.Query
import Jordan.Servant.Query.Parse
import Jordan.Types.JSONError
import Network.HTTP.Media (matchContent)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request (..), lazyRequestBody, queryString)
import Servant.API
import Servant.API.Modifiers
import Servant.Server
import Servant.Server.Internal
import Servant.Server.Internal.ServerError
import Servant.Server.UVerb

instance forall a rest context. (HasServer rest context, FromJSON a) => HasServer (ReportingRequestBody a :> rest) context where
  type ServerT (ReportingRequestBody a :> rest) m = a -> ServerT rest m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy rest) pc nt . s
  route (Proxy :: Proxy (ReportingRequestBody a :> rest)) context subserver =
    route (Proxy :: Proxy rest) context $ addBodyCheck subserver checkContent checkBody
    where
      checkContent = withRequest $ \request -> do
        let contentType = fromMaybe "application/octect-stream" $ lookup hContentType $ requestHeaders request
        case matchContent (toList $ contentTypes (Proxy :: Proxy JordanJSON)) contentType of
          Nothing -> delayedFail err415
          Just _ -> pure (parseJSONReporting @a)
      checkBody parser = withRequest $ \request -> do
        body <- liftIO $ lazyRequestBody request
        case parser (LBS.toStrict body) of
          Left je ->
            delayedFailFatal $
              ServerError
                { errHTTPCode = 400,
                  errReasonPhrase = "Bad Request",
                  errBody = toJSONViaBuilder je,
                  errHeaders = [("Content-Type", "application/json+haskell-servant-body-error")]
                }
          Right a -> pure a

-- | If the query was required, no maybe.
-- Otherwise, wrap the query in a maybe.
type QueryArgument mods a = If (FoldRequired mods) a (Maybe a)

instance
  forall a rest context baseKey mods.
  ( HasServer rest context,
    FromJSON a,
    KnownSymbol baseKey,
    SBoolI (FoldRequired mods)
  ) =>
  HasServer (JordanQuery' baseKey mods a :> rest) context
  where
  type ServerT (JordanQuery' baseKey mods a :> rest) m = QueryArgument mods a -> ServerT rest m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy rest) pc nt . s
  route (Proxy :: Proxy (JordanQuery' baseKey mods a :> rest)) context subserver =
    route (Proxy :: Proxy rest) context $ subserver `addParameterCheck` withRequest paramsCheck
    where
      keyName = T.pack $ symbolVal (Proxy :: Proxy baseKey)
      parseQ :: Request -> Either String a
      parseQ = parseQueryAtKey keyName . queryString
      hasQuery = hasQueryAtKey keyName . queryString
      failWithError s =
        delayedFailFatal $
          err400
            { errBody = toJSONViaBuilder s,
              errHeaders = [("Content-Type", "application/json+haskell-jordan-query-error")]
            }
      paramsCheck req =
        let parsed = parseQ req
            hasKeys = hasQuery req
         in case sbool @(FoldRequired mods) of
              STrue -> either failWithError pure parsed
              SFalse
                | not hasKeys -> pure Nothing
                | otherwise -> either failWithError (pure . Just) parsed
