{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant.Query
  ( JordanQuery',
    OptionalJordanQuery,
    RequiredJordanQuery,

    -- * Parsing queries
    parseQueryAtKey,
    parseQueryAtKeyWith,
    hasQueryAtKey,

    -- * Rendering queries
    renderQueryAtKey,
    renderQueryAtKeyWith,
  )
where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)
import Jordan.Servant.Query.Parse
  ( hasQueryAtKey,
    parseQueryAtKey,
    parseQueryAtKeyWith,
  )
import Jordan.Servant.Query.Render
  ( renderQueryAtKey,
    renderQueryAtKeyWith,
  )
import Servant.API.Modifiers (Required)

-- | A query argument at some key, that will be parsed via Jordan.
-- If the query needs to contain nested data, it will all be nested under the same key.
--
-- We do not support lenient queries as figuring out what to return in the case where the Jordan parser
-- would have parsed nested keys is too difficult.
type JordanQuery' :: Symbol -> [*] -> * -> *
data JordanQuery' baseStr required a

-- | A query argument that is required.
--
-- Will render an error message, in JSON format, if the query was bad in some way.
type RequiredJordanQuery bs a = JordanQuery' bs '[Required] a

-- | A query argument that is *optional*.
--
-- Will render an error message, in JSON format, if the query was bad in some way.
type OptionalJordanQuery bs a = JordanQuery' bs '[] a
