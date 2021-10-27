{-# LANGUAGE DeriveGeneric #-}
module Jordan.Generic.Options
    where

import GHC.Generics (Generic)

data SumTypeEncoding
  = TagVal
  | TagInField
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

newtype PartOfSum f a = PartOfSum { getPartOfSum :: f a }
