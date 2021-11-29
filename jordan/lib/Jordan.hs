-- | Base module!
--
-- Has all functionality for parsing and serializing JSON.
module Jordan
    ( -- * JSON Parsing
      -- ** Concretely
      parseViaMegaparsec
    , parseViaAttoparsec
    , runParserViaAttoparsec
    , runParserViaMegaparsec
    , attoparsecParser
    , megaparsecParser
      -- ** Abstractly
    , FromJSON (..)
    , JSONParser (..)
    , JSONObjectParser (..)
    , JSONTupleParser (..)
      -- *** Generically
    , gFromJSON
    , FromJSONOptions (..)
      -- * JSON Serialization
      -- ** Concretely
    , toJSONAsBuilder
    , toJSONViaBuilder
    , toJSONText
      -- ** Abstractly
    , ToJSON (..)
    , JSONSerializer (..)
    , JSONObjectSerializer (..)
      -- *** Generically
    , gToJSON
    , ToJSONOptions (..)
      -- * Parsing or Serializing Arbitrary JSON
    , JSONValue (..)
    ) where

import Jordan.FromJSON.Attoparsec
import Jordan.FromJSON.Class
import Jordan.FromJSON.Megaparsec
import Jordan.JSONValue
import Jordan.ToJSON.Builder (toJSONAsBuilder, toJSONViaBuilder)
import Jordan.ToJSON.Class
import Jordan.ToJSON.Text (toJSONText)
