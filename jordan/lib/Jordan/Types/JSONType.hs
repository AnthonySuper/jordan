{-# LANGUAGE DeriveGeneric #-}

module Jordan.Types.JSONType where

import Control.DeepSeq
import GHC.Generics
import Jordan.FromJSON.Class (FromJSON)
import Jordan.ToJSON.Class (ToJSON)

data JSONType
  = JSONTypeNull
  | JSONTypeBool
  | JSONTypeText
  | JSONTypeNumber
  | JSONTypeArray
  | JSONTypeObject
  deriving (Show, Eq, Read, Ord, Bounded, Enum, Generic)

instance ToJSON JSONType

instance FromJSON JSONType

instance NFData JSONType
