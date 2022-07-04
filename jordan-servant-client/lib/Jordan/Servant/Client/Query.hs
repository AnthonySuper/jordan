{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant.Client.Query where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Exts (IsList (..))
import GHC.TypeLits
import Jordan
import Jordan.Servant.Query
import Jordan.Servant.Query.Render
import Servant.API
import Servant.API.Modifiers
import Servant.Client.Core

-- | Note: this instances assumes that the Jordan.FromJSON and Jordan.ToJSON instances match.
instance
  forall a sym m api mods.
  (KnownSymbol sym, ToJSON a, HasClient m api, SBoolI (FoldRequired mods)) =>
  HasClient m (JordanQuery' sym mods a :> api)
  where
  type Client m (JordanQuery' sym mods a :> api) = RequiredArgument mods a -> Client m api
  clientWithRoute pm Proxy req mparam =
    clientWithRoute pm (Proxy @api) $ foldRequiredArgument (Proxy @mods) add (maybe req add) mparam
    where
      add :: a -> Request
      add param =
        req {requestQueryString = requestQueryString req <> newItems}
        where
          newItems =
            fromList $
              renderQueryAtKey
                (T.pack $ symbolVal $ Proxy @sym)
                param
  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy @api) f . cl
