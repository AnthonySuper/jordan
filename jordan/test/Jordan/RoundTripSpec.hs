{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Jordan.RoundTripSpec
    where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import Jordan (parseViaAttoparsec, parseViaMegaparsec, toJSONText, toJSONViaBuilder)
import Jordan.FromJSON.Class (FromJSON(..), JSONParser(..))
import Jordan.ToJSON.Class (JSONSerializer(..), ToJSON(..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Utf8

makeResult
  :: (ToJSON a, FromJSON a, Arbitrary a, Show a, Eq a)
  => Proxy a
  -> (a -> b)
  -> (b -> String)
  -> (b -> Either String a)
  -> Property
makeResult (Proxy :: Proxy a) convForward convString convBack =
  forAllShow (arbitrary @a) showResult convert
      where
        showResult a
          = show a
          <> "\n"
          <> convString (convForward  a)
          <> "\n"
          <> showError (convBack $ convForward a)
        convert a = convBack (convForward a) == pure a
        showError :: Either String a -> String
        showError (Left err) = "Error\n" <> err
        showError (Right a) = "Success: " <> show a

newtype ExtremelyBasic
  = ExtremelyBasic { getExtremelyBasic :: () }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ExtremelyBasic where
  arbitrary = ExtremelyBasic <$> arbitrary

data TwoFieldsRec
  = TwoFieldsRec { firstField :: Int, secondField :: Int }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary TwoFieldsRec where
  arbitrary = TwoFieldsRec <$> arbitrary <*> arbitrary

data RLMSum
  = Mike
  | Jay
  | Rich
  | Jack
  | Josh
  deriving (Eq, Show, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary RLMSum where
  arbitrary = arbitraryBoundedEnum

data ManyChoices
  = ChoseFirst { getFirst :: Int }
  | ChoseSecond { getSecondA :: Int, getSecondB :: Int }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ManyChoices where
  arbitrary
    = oneof
    [ ChoseFirst <$> arbitrary
    , ChoseSecond <$> arbitrary <*> arbitrary
    ]

data FakePerson
  = FakePerson
  { age :: Int
  , name :: String
  , cool :: Bool
  } deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary FakePerson where
  arbitrary
    = FakePerson
    <$> arbitrary
    <*> fmap unpack genValidUtf8
    <*> arbitrary

newtype OnlyText = OnlyText { getText :: Text }
  deriving (Show, Eq)

instance ToJSON OnlyText where
  toJSON = contramap getText serializeText

instance FromJSON OnlyText where
  fromJSON = OnlyText <$> parseText

instance Arbitrary OnlyText where
  arbitrary = OnlyText <$> genValidUtf8

data EnumyObject
  = EnumA
  | EnumB
  | EnumC
  | EnumObject { enumValue :: Text }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary EnumyObject where
  arbitrary
    = oneof
    [ pure EnumA
    , pure EnumB
    , pure EnumC
    , EnumObject <$> genValidUtf8
    ]

data AllTogether
  = AllTogether
  { extremelyBasic :: ExtremelyBasic
  , twoFieldsRec :: TwoFieldsRec
  , rlmSum :: RLMSum
  , fakePerson :: FakePerson
  , onlyText :: OnlyText
  , enumyObject :: EnumyObject
  } deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary AllTogether where
  arbitrary
    = AllTogether
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

showViaText :: (ToJSON a) => a -> String
showViaText = unpack . toJSONText

showViaBuilder :: (ToJSON a) => a -> String
showViaBuilder = unpack . decodeUtf8 . toStrict . toJSONViaBuilder

roundtrips
  :: (Arbitrary a, Show a, Eq a, ToJSON a, FromJSON a)
  => String
  -> Proxy a
  -> Spec
roundtrips n p = describe ("round-trippping " <> n) $ do
  describe "when serializing via text" $ do
    let q = makeResult p toJSONText unpack
    prop "roundtrips back via megaparsec" $
      q parseViaMegaparsec
    prop "roundtrips back via attoparsec" $
      q (parseViaAttoparsec . encodeUtf8)
  describe "when serializing via a builder" $ do
    let q = makeResult p toJSONViaBuilder (unpack . decodeUtf8 . toStrict)
    prop "roundtrips back via megaparsec" $
      q (parseViaMegaparsec . decodeUtf8 . toStrict)
    prop "roundtrips back via attoparsec" $
      q (parseViaAttoparsec . toStrict)

spec :: Spec
spec = describe "round-tripping generic values" $ do
  roundtrips "a newtype around ()" (Proxy @ExtremelyBasic)
  roundtrips "a record with two int fields" (Proxy @TwoFieldsRec)
  roundtrips "a sum enum type" (Proxy @RLMSum)
  roundtrips "a type with some text" (Proxy @FakePerson)
  roundtrips "just text" (Proxy @OnlyText)
  roundtrips "an object with some constructors as enums" (Proxy @EnumyObject)
  roundtrips "an object composed of all rountrip'd objects" (Proxy @AllTogether)
