{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Jordan.OpenAPI.SpecDefs
    where

import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Jordan (FromJSON(..), ToJSON(..))

data Person
  = Person
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data Managership
  = Managership
  { manager :: Person
  , underlings :: [Managership]
  , feelings :: Map.Map T.Text T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Rank
  = RankNested { top :: T.Text, nest :: Rank }
  | RankSplit { left :: Rank, right :: Rank }
  | RankBottom { label :: T.Text }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FileEntry
  = FileEntry
  { name :: T.Text
  , contents :: Either File Directory
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype File = File { getFile :: T.Text }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Directory = Directory { getEntries :: [FileEntry] }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
