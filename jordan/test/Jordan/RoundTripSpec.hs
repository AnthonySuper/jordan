{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Jordan.RoundTripSpec where

import Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Contravariant (Contravariant (..))
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Proxy (..), Typeable)
import GHC.Generics
import Jordan (parseViaAttoparsec, toJSONViaBuilder)
import Jordan.FromJSON.Class (FromJSON (..), GFromJSON (..), JSONParser (..))
import Jordan.FromJSON.UnboxedReporting (parseOrReport)
import Jordan.Generic.Options
import Jordan.ToJSON.Class (GToJSON (..), JSONSerializer (..), ToJSON (..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Utf8

data PropertyResult exp = MkPropertyResult
  { expectedValue :: exp,
    actualValue :: Either String exp,
    jsonStringTested :: Text
  }
  deriving (Show, Read)

roundtripProperty ::
  (ToJSON a, FromJSON a, Arbitrary a, Show a, Eq a, Eq err, Show err) =>
  Proxy a ->
  (ByteString -> Either err a) ->
  (err -> String) ->
  Property
roundtripProperty (Proxy :: Proxy a) parser mapErr =
  forAllShrink (arbitrary @a) (shrink @a) $ \a ->
    let built = toJSONViaBuilder a
     in counterexample (show built) $ parser (toJSONViaBuilder a) === Right a

makeResult ::
  (ToJSON a, FromJSON a, Arbitrary a, Show a, Eq a) =>
  Proxy a ->
  (a -> b) ->
  (b -> String) ->
  (b -> Either String a) ->
  Property
makeResult (Proxy :: Proxy a) convForward convString convBack =
  forAllShow (arbitrary @a) showResult convert
  where
    showResult a =
      show a
        <> "\n"
        <> convString (convForward a)
        <> "\n"
        <> showError (convBack $ convForward a)
    convert a = convBack (convForward a) == pure a
    showError :: Either String a -> String
    showError (Left err) = "Error\n" <> err
    showError (Right a) = "Success: " <> show a

newtype ExtremelyBasic = ExtremelyBasic {getExtremelyBasic :: ()}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ExtremelyBasic where
  arbitrary = ExtremelyBasic <$> arbitrary

data TwoFieldsRec = TwoFieldsRec {firstField :: Int, secondField :: Int}
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

data TwoChoices
  = ChoseFirst {getFirst :: Int}
  | ChoseSecond {getSecondA :: Int, getSecondB :: Int}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary TwoChoices where
  arbitrary =
    oneof
      [ ChoseFirst <$> arbitrary,
        ChoseSecond <$> arbitrary <*> arbitrary
      ]

data FakePerson = FakePerson
  { age :: Int,
    name :: String,
    cool :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Arbitrary FakePerson where
  arbitrary =
    FakePerson
      <$> arbitrary
      <*> fmap unpack genValidUtf8
      <*> arbitrary

newtype OnlyText = OnlyText {getText :: Text}
  deriving (Show, Eq, Generic)

shrinkText :: Text -> [Text]
shrinkText t
  | Text.length t == 0 = []
  | Text.length t == 1 = [""]
  | otherwise = Text.singleton <$> Text.unpack t

instance ToJSON OnlyText where
  toJSON = contramap getText serializeText

instance FromJSON OnlyText where
  fromJSON = OnlyText <$> parseText

instance Arbitrary OnlyText where
  arbitrary = OnlyText <$> genValidUtf8
  shrink (OnlyText t) = OnlyText <$> shrinkText t

data EnumyObject
  = EnumA
  | EnumB
  | EnumC
  | EnumObject {enumValue :: Text}
  | EnumOther OnlyText
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary EnumyObject where
  arbitrary =
    oneof
      [ pure EnumA,
        pure EnumB,
        pure EnumC,
        EnumObject <$> genValidUtf8,
        EnumOther <$> arbitrary
      ]
  shrink (EnumObject o) = EnumObject <$> shrinkText o
  shrink (EnumOther ot) = EnumOther <$> shrink ot
  shrink _ = []

data AllTogether = AllTogether
  { extremelyBasic :: ExtremelyBasic,
    twoFieldsRec :: TwoFieldsRec,
    rlmSum :: RLMSum,
    fakePerson :: FakePerson,
    onlyText :: OnlyText,
    enumyObject :: EnumyObject
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary AllTogether where
  arbitrary =
    AllTogether
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (AllTogether ub tfr rs fp ot eo) =
    AllTogether
      <$> shrink ub
      <*> shrink tfr
      <*> shrink rs
      <*> shrink fp
      <*> shrink ot
      <*> shrink eo

data AnnoyinglyOptional = AnnoyinglyOptional
  { annoyingFirst :: Maybe Int,
    annoyingSecond :: Maybe Int,
    annoyingThird :: Maybe Int
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary AnnoyinglyOptional where
  arbitrary =
    pure $ AnnoyinglyOptional Nothing Nothing Nothing

-- <$> arbitrary <*> arbitrary <*> arbitrary

data JustNulls = JustNulls
  { jna :: (),
    jnb :: (),
    jnc :: (),
    jnd :: (),
    jne :: (),
    jnf :: ()
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary JustNulls where
  arbitrary =
    pure $
      JustNulls () () () () () ()

showViaText :: (ToJSON a) => a -> String
showViaText = unpack . decodeUtf8 . LBS.toStrict . toJSONViaBuilder

showViaBuilder :: (ToJSON a) => a -> String
showViaBuilder = unpack . decodeUtf8 . toStrict . toJSONViaBuilder

showLeft :: (Show a) => Either a b -> Either [Char] b
showLeft = \case
  Left a -> Left (show a)
  Right b -> Right b

instance Arbitrary a => Arbitrary (WithOptions opts a) where
  arbitrary = WithOptions <$> arbitrary
  shrink (WithOptions a) = WithOptions <$> shrink a

roundtrips' ::
  (Arbitrary a, Show a, Eq a, ToJSON a, FromJSON a) =>
  Proxy a ->
  Spec
roundtrips' p = do
  describe "when serializing via a builder" $ do
    prop "roundtrips back via attoparsec" $
      roundtripProperty p (parseViaAttoparsec . toStrict) show
    prop "roundstrips back via unboxed reporting" $
      roundtripProperty p (parseOrReport . toStrict) show

roundtrips ::
  ( Arbitrary a,
    Typeable a,
    Show a,
    Eq a,
    ToJSON a,
    FromJSON a,
    Generic a,
    GToJSON (Rep a),
    GFromJSON (Rep a)
  ) =>
  String ->
  Proxy a ->
  Spec
roundtrips n p@(Proxy :: Proxy a) = describe ("round-tripping " <> n) $ do
  describe "default roundtrips" $
    roundtrips' p
  describe "roundtrips with no options" $
    roundtrips' (Proxy @(WithOptions '[] a))
  describe "Roundtrips omitting nothing fields" $
    roundtrips' (Proxy @(WithOptions '[OmitNothingFields] a))
  describe "Roundtrips keeping nothing fields" $
    roundtrips' (Proxy @(WithOptions '[KeepNothingFields] a))

spec :: Spec
spec = describe "round-tripping generic values" $ do
  roundtrips "a newtype around ()" (Proxy @ExtremelyBasic)
  roundtrips "a record with two int fields" (Proxy @TwoFieldsRec)
  roundtrips "a sum enum type" (Proxy @RLMSum)
  roundtrips "a type with some text" (Proxy @FakePerson)
  roundtrips "just text" (Proxy @OnlyText)
  roundtrips "an object with some constructors as enums" (Proxy @EnumyObject)
  roundtrips "an object composed of all rountrip'd objects" (Proxy @AllTogether)
  roundtrips "an object with optional fields" (Proxy @AnnoyinglyOptional)
  roundtrips "an object that is jut a bunch of nulls" (Proxy @JustNulls)
