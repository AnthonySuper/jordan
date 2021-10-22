{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import qualified Data.Map as Map
import Data.OpenApi.Declare
import Data.OpenApi.Internal
import Data.OpenApi.Internal.Utils (encodePretty)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as TIO
import GHC.Generics
import Jordan.FromJSON.Class
import Jordan.OpenAPI
import Jordan.ToJSON.Class

data Person
  = Person
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show, Generic)

data Managership
  = Managership { manager :: Person, underlings :: [Managership], feelings :: Map.Map T.Text T.Text }
  deriving (Show, Generic)

data Rank
  = RankNested { top :: T.Text, nest :: Rank }
  | RankSplit { left :: Rank, right :: Rank }
  | RankBottom
  deriving (Show, Generic)

data FileEntry
  = FileEntry { name :: T.Text, contents :: Either File Directory }
  deriving (Show, Eq, Ord, Generic)

newtype File = File { getFile :: T.Text }
  deriving (Show, Eq, Ord, Generic)

newtype Directory = Directory { getEntries :: [FileEntry] }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Person
instance FromJSON Managership
instance FromJSON Rank

instance ToJSON FileEntry
instance ToJSON Directory
instance ToJSON File

instance FromJSON FileEntry where
  fromJSON = parseObject "FileEntry" $
    FileEntry <$> parseField "name" <*> parseField "contents"

instance FromJSON File where

instance FromJSON Directory where

declared :: Declare (Definitions Schema) Schema
declared = fmap _namedSchemaSchema (getFromNamed (Proxy :: Proxy Managership))

main :: IO ()
main = TIO.putStrLn $ decodeUtf8 $ encodePretty $ runDeclare declared mempty
