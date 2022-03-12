-- | Base module!
--
-- Has all functionality for parsing and serializing JSON.
module Jordan
  ( -- * JSON Parsing

    -- ** Abstractly
    FromJSON (..),
    JSONParser (..),
    JSONObjectParser (..),
    JSONTupleParser (..),

    -- ** Concretely

    -- *** Via Attoparsec
    -- $viaAP
    parseViaAttoparsec,
    parseViaAttoparsecWith,
    attoparsecParser,
    attoparsecParserFor,

    -- *** With Error Reporting
    -- $withReport
    parseOrReport,
    parseOrReportWith,

    -- *** Generically
    gFromJSON,
    FromJSONOptions (..),

    -- * JSON Serialization

    -- ** Abstractly
    ToJSON (..),
    JSONSerializer (..),
    JSONObjectSerializer (..),

    -- **** Re-Exports for Serialization
    Contravariant (..),
    Divisible (..),
    Selectable (..),

    -- ** Concretely
    toJSONAsBuilder,
    toJSONViaBuilder,

    -- *** Generically
    gToJSON,
    ToJSONOptions (..),

    -- * Parsing or Serializing Arbitrary JSON
    JSONValue (..),

    -- * Newtypes for DerivingVia
    WithOptions (..),
    OmitNothingFields,
    KeepNothingFields,
  )
where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Jordan.FromJSON.Attoparsec
import Jordan.FromJSON.Class
import Jordan.FromJSON.UnboxedReporting
import Jordan.Generic.Options
import Jordan.ToJSON.Builder (toJSONAsBuilder, toJSONViaBuilder)
import Jordan.ToJSON.Class
import Jordan.Types.JSONValue (JSONValue (..))

-- $viaAP
--
-- These parsers use the excellent Attoparsec library to do their work.
-- This means that they're quite fast, but that they also provide less-than-ideal error messages.
-- You should use these when speed is needed, or when you're reasonably certain that nobody will make a mistake.
-- APIs intended only for internal use, for example.

-- $withReport
--
-- These parsers parse to either a value or an *error report*, which is a detailed report of what exactly what wrong.
-- This uses a roll-our-own parsing library based on *unboxed sums*.
-- It's been tested via QuickCheck, but it is doing some spooky-scary raw pointer opertions.
--
-- This is a bit slower than the attoparsec parser, but *much* better at error handling.
-- Use it for external-facing APIs---assuming that you trust my ability to write primops.
