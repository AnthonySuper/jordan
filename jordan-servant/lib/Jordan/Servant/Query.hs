{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Servant.Query
  ( -- * Servant Combinators
    JordanQuery',
    OptionalJordanQuery,
    RequiredJordanQuery,

    -- * Using Jordan with query strings
    -- $queries

    -- ** Parsing Queries
    parseQueryAtKey,
    parseQueryAtKeyWith,
    hasQueryAtKey,

    -- ** Rendering queries
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

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> import GHC.Generics
-- >>> import Jordan
-- >>> import Network.HTTP.Types.URI
-- >>> data Person = Person { firstName :: String, lastName :: String } deriving (Show, Read, Eq, Ord, Generic)
-- >>> instance FromJSON Person
-- >>> instance ToJSON Person

-- $queries
--
-- This module provides a way to use Jordan to parse to or render from query strings.
--
-- An example is helpful:
--
-- >>> renderQueryAtKey "person" (Person { firstName = "Rich", lastName = "Evans" })
-- [("person[firstName]",Just "Rich"),("person[lastName]",Just "Evans")]
--
-- >>> renderQuery True $ renderQueryAtKey "person" (Person { firstName = "Rich", lastName = "Evans" })
-- "?person%5BfirstName%5D=Rich&person%5BlastName%5D=Evans"
--
-- >>> parseQueryAtKey @Person "person" $ renderQueryAtKey "person" (Person { firstName = "Mike", lastName = "Stoklassa" })
-- Right (Person {firstName = "Mike", lastName = "Stoklassa"})
--
-- The format of parsed and rendered queries is designed to be \"similar enough\" to how Rails does it, which is
-- also used in several other libraries.

-- | A query argument at some key, that will be parsed via Jordan.
-- If the query needs to contain nested data, it will all be nested under the same key.
--
-- We do not support lenient queries as figuring out what to return in the case where the Jordan parser
-- would have parsed nested keys is too difficult.
--
-- Note: this type *does not* have a 'HasLink' instance, because unfortunately Servant is way too restrictive of what it exports,
-- making such an instance impossible to write. I will open up a PR against Servant to fix this soon.
data JordanQuery' (baseStr :: Symbol) (options :: [*]) (a :: *)

-- | A query argument that is required.
--
-- Will render an error message, in JSON format, if the query was bad in some way.
type RequiredJordanQuery (baseStr :: Symbol) (a :: *) = JordanQuery' baseStr '[Required] a

-- | A query argument that is *optional*.
--
-- Will render an error message, in JSON format, if the query was bad in some way.
type OptionalJordanQuery (baseStr :: Symbol) (a :: *) = JordanQuery' (baseStr :: Symbol) '[] (a :: *)
