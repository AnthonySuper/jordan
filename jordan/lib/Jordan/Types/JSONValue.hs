{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Jordan.Types.JSONValue
    ( JSONValue (..)
    ) where

import Data.Functor (($>))
import Data.Functor.Contravariant (Contravariant(..))
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic(..))
import Jordan.FromJSON.Class (FromJSON(..), JSONParser(..))
import Jordan.ToJSON.Class (JSONSerializer(..), Selectable(..), ToJSON(..), selected)

-- | A type for any JSON value.
-- This is a basic Haskell sum type representation.
--
-- This is intended to for use when working with JSON where you do not know much about its structure.
data JSONValue
  = JNull
  | JBool Bool
  | JText Text
  | JNumber Scientific
  | JArray [JSONValue]
  | JObject (Map.Map Text JSONValue)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON JSONValue where
  fromJSON
    = (parseNull $> JNull)
    <> (JText <$> parseText)
    <> (JBool <$> parseBool)
    <> (JNumber <$> parseNumber)
    <> nameParser "Jordan.JSONValue.Array.Input" (JArray <$> parseArrayWith fromJSON)
    <> nameParser "Jordan.JSONValue.Map.Input" (JObject . Map.fromList <$> parseDictionary fromJSON)

type AsEither
  = Either ()
      (Either Bool
        (Either Text
          (Either Scientific
            (Either [JSONValue] (Map.Map Text JSONValue)))))

toNestedEither
  :: JSONValue
  -> AsEither
toNestedEither = \case
  JNull -> Left ()
  JBool b -> Right (Left b)
  JText txt -> Right (Right (Left txt))
  JNumber sci -> Right (Right (Right (Left sci)))
  JArray jvs -> Right (Right (Right (Right (Left jvs))))
  JObject map -> Right (Right (Right (Right (Right map))))

instance ToJSON JSONValue where
  toJSON = select toNestedEither serializeNull s1
    where
      s1 = selected serializeBool s2
      s2 = selected serializeText s3
      s3 = selected serializeNumber s4
      s4
        = selected (nameSerializer "Jordan.JSONValue.Array.Output" serializeArray)
        $ nameSerializer "Jordan.JSONValue.Map.Output"
        $ contramap Map.toList
        $ serializeDictionary toJSON
