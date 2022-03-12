{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Jordan.Servant.Query.ParseSpec where

import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import Jordan.FromJSON.Class
import Jordan.Servant.Query
import Jordan.Servant.Query.Parse
import Network.HTTP.Types.URI
import Test.Hspec

data CategoryName = CategoryName {category :: Text, name :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

data CoolFilter = CoolFilter {filterName :: Maybe Text, filterDesc :: Maybe Text}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CoolFilter where
  fromJSON =
    parseObject $
      CoolFilter
        <$> parseFieldWithDefault "name" (Just <$> fromJSON) Nothing
        <*> parseFieldWithDefault "desc" (Just <$> fromJSON) Nothing

newtype CoolFilters = CoolFilters {getCoolFilters :: [CoolFilter]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON CoolFilters where
  fromJSON = CoolFilters <$> fromJSON

data IntRange
  = UnboundedAfter Int
  | UnboundedBefore Int
  | Within Int Int
  deriving (Show, Eq, Ord, Generic)

instance FromJSON IntRange where
  fromJSON =
    parseObject (Within <$> parseField "s" <*> parseField "e")
      <> parseObject (UnboundedAfter <$> parseField "s")
      <> parseObject (UnboundedBefore <$> parseField "e")

newtype IntMultiRange = IntMultiRange {getRanges :: [IntRange]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON IntMultiRange where
  fromJSON = IntMultiRange <$> fromJSON

newtype FilterValues = FilterValues {getFilterValues :: [(Text, Int)]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON FilterValues where
  fromJSON = FilterValues <$> parseDictionary fromJSON

newtype DictRange = DictRange {getDictRange :: [(Text, IntRange)]}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DictRange where
  fromJSON = DictRange <$> parseDictionary fromJSON

parsesQTo :: (Show a, Eq a, FromJSON a) => ByteString -> a -> SpecWith ()
parsesQTo bs a =
  it ("parses query string " <> show bs <> " to " <> show a) $
    parseQueryAtKey "q" (parseQueryReplacePlus False bs) `shouldBe` Right a

spec :: Spec
spec = describe "Jordan.Servant.Query" $ do
  queryParsing
  stringParsing

stringParsing :: Spec
stringParsing = describe "parsing query strings" $ do
  describe "basic single-field parsing" $ do
    "q=10" `parsesQTo` (10 :: Int)
    "q=foo" `parsesQTo` ("foo" :: Text)
  describe "object parsing with no defaults" $ do
    "q[category]=posts&q[name]=bob" `parsesQTo` CategoryName "posts" "bob"
    "q[name]=joe&q[category]=videos" `parsesQTo` CategoryName "videos" "joe"
  describe "array parsing" $ do
    "q[]=1" `parsesQTo` [1 :: Int]
    "q[]=1&q[]=2" `parsesQTo` [1 :: Int, 2]
  describe "object parsing with two defaults" $ do
    "q[s]=100" `parsesQTo` UnboundedAfter 100
    "q[e]=10" `parsesQTo` UnboundedBefore 10
    "q[e]=10&q[s]=1" `parsesQTo` Within 1 10
  describe "arrays of object parsing" $ do
    "q[][s]=1&q[][e]=10&q[][s]=25" `parsesQTo` IntMultiRange [Within 1 10, UnboundedAfter 25]
  describe "arrays of non-failing objects" $ do
    "q[][name]=wow" `parsesQTo` CoolFilters [CoolFilter (Just "wow") Nothing]
    "" `parsesQTo` CoolFilters []
  describe "simple dictionaries" $ do
    "q[foo]=10&q[bar]=11" `parsesQTo` FilterValues [("foo", 10), ("bar", 11)]
    "" `parsesQTo` FilterValues []
    "q[foo]=100" `parsesQTo` FilterValues [("foo", 100)]
  describe "dictionaries of objects" $ do
    "q[var][e]=10&q[bar][s]=11&q[bar][e]=100" `parsesQTo` DictRange [("var", UnboundedBefore 10), ("bar", Within 11 100)]

parsesAsKey :: ByteString -> QueryKey -> SpecWith ()
parsesAsKey input output =
  it ("should parse " <> show input <> " to query key " <> show output) $
    AP.parseOnly parseQueryKey input `shouldBe` Right output

queryParsing :: Spec
queryParsing = describe "parsing query keys" $ do
  "q" `parsesAsKey` QueryKey [RawValue "q"]
  "q[[" `parsesAsKey` QueryKey [RawValue "q["]
  "q[bar]" `parsesAsKey` QueryKey [RawValue "q", BracedValue "bar"]
  "q[bar][][baz]" `parsesAsKey` QueryKey [RawValue "q", BracedValue "bar", EmptyBraces, BracedValue "baz"]
  "q[][][]" `parsesAsKey` QueryKey [RawValue "q", EmptyBraces, EmptyBraces, EmptyBraces]
  "q[]bar" `parsesAsKey` QueryKey [RawValue "q", EmptyBraces, RawValue "bar"]
  "q[]]foo]" `parsesAsKey` QueryKey [RawValue "q", BracedValue "]foo"]
  "q[\\[foo]" `parsesAsKey` QueryKey [RawValue "q", BracedValue "[foo"]
