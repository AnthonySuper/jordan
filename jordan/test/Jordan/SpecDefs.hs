{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Jordan.SpecDefs
    where

import Control.Applicative (Alternative((<|>)))
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.String (IsString(..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Jordan.FromJSON.Class
import Jordan.ToJSON.Class
import Test.Hspec (Arg, Example, Spec, SpecWith, describe, it, shouldBe)
import Text.RawString.QQ

newtype BasicStruct
  = BasicStruct { bar :: () }
  deriving (Show, Eq, Ord)

goodBasic :: BasicStruct
goodBasic = BasicStruct ()

instance FromJSON BasicStruct where
  fromJSON = (BasicStruct <$> fromNull) <> fromObject
    where
      fromObject = parseObject "TestItems.BasicStruct.Output" $
        BasicStruct <$> parseField "bar"
      fromNull = parseNull

instance ToJSON BasicStruct where
  toJSON = serializeObject "TestItems.BasicStruct.Input" $ writeField "bar" serializeNull

basicNull = pack [r| null |]
basicOneField = pack [r| { "bar": null } |]
basicEscaped = pack [r| { "\u0062\u0061\u0072": null } |]
basicPartialEscape = pack [r| { "\u0062a\u0072": null } |]

basicExtraFields = pack [r| { "whatever": null, "bar": null, "baz": null } |]

basicArray = pack [r| [null, { "whatever": null, "bar": null }, null, null] |]

data TwoFields
  = TwoFields { one :: (), two :: () }
  deriving (Show, Eq, Ord, Generic)

goodTwo :: TwoFields
goodTwo = TwoFields () ()

instance FromJSON TwoFields where
  fromJSON = parseObject "TwoFields" $ TwoFields <$> parseField "one" <*> parseField "two"

twoSimple = pack [r| { "one": null, "two": null } |]

twoScramble = pack [r| { "two": null, "one": null } |]

twoExtra = pack [r|
  {
    "ignore": [],
    "one": null,
    "four": [],
    "five": "test",
    "seven": {},
    "six": "why tho",
    "two": null,
    "three": {},
    "ignored": [1,2,3,4, { " foo "   : null }]
  }
|]

twoScrambleExtra = pack [r|
  {
    "ignore": null,
    "bad": null,
    "two": null,
    "another": null,
    "yetAgain": null,
    "one": null
  }
|]

data GenericStruct
  = GenericStruct
  { firstLabel :: ()
  , secondLabel :: [()]
  } deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON GenericStruct

genericDefault = pack [r| { "firstLabel": null, "secondLabel": [null] } |]

data GenericSum
  = GenericBasic BasicStruct
  | GenericTwo TwoFields
  deriving (Show, Eq, Ord, Generic)

instance FromJSON GenericSum

sumBasic = pack [r| { "GenericBasic": null } |]
sumBasicObj = pack [r| { "GenericBasic": { "bar": null } } |]

sumTwo = pack [r| { "GenericTwo": { "one": null, "two": null, "bar": null } } |]
mismatchType = pack [r| { "GenericTwo": null } |]

basicWritingSpec
  :: (forall val. (ToJSON val, Show val) => val -> Text)
  -> Spec
basicWritingSpec writeJSON = do
  describe "writing basic primitives" $ do
    it "writes nulls" $
      writeJSON () `shouldBe` "null"
    it "writes True to true" $
      writeJSON True `shouldBe` "true"
    it "writes False to false" $
      writeJSON False `shouldBe` "false"
    it "writes an empty array" $
      writeJSON ([] :: [()]) `shouldBe` "[]"
    it "writes an array with one item" $ do
      writeJSON [()] `shouldBe` "[null]"
    it "writes an array with two items" $ do
      writeJSON [(),()] `shouldBe` "[null,null]"
    it "writes the number 1" $ do
      writeJSON (1 :: Int) `shouldBe` "1.0"
    it "writes the number 1.5" $ do
      writeJSON (1.5 :: Double) `shouldBe` "1.5"

  describe "writing text" $ do
    it "writes with no escapes" $ do
      writeJSON ("foo" :: Text) `shouldBe` [r|"foo"|]
    it "writes with an escaped quote" $ do
      writeJSON ("foo\"" :: Text) `shouldBe` [r|"foo\""|]
    it "writes with an escaped backslash" $ do
      writeJSON ("foo\\" :: Text) `shouldBe` [r|"foo\\"|]
  describe "writing a basic object" $ do
    it "writes properly" $ do
      writeJSON goodBasic `shouldBe` [r|{"bar": null}|]

basicParsingSpec
  :: (Example a)
  => (forall val. (FromJSON val, Show val, Eq val) => Text -> val -> a)
  -> SpecWith (Arg a)
basicParsingSpec parseMatch = do
  describe "parsing basic structure" $ do
    it "parses true" $
      "true" `parseMatch` True
    it "parses false" $
      "false" `parseMatch` False
    it "parses a raw null" $
      "null" `parseMatch` ()
    it "parses an empty array of nulls" $
      "[]" `parseMatch` ([] :: [()])
    it "parses a one-null array" $ do
      "[null ]" `parseMatch` [()]
    it "parses a two-null array with weird spaces" $ do
      "[null\n,null]" `parseMatch` [(), ()]
    it "parses null to a basic struct" $
      basicNull `parseMatch` goodBasic
    it "parses with field" $
      basicOneField `parseMatch` goodBasic
    it "parses with two fields" $
      basicExtraFields `parseMatch` goodBasic
    it "parses with a fully-escaped field" $
      basicEscaped `parseMatch` goodBasic
    it "parses with a partially-escaped field" $
      basicPartialEscape `parseMatch` goodBasic
    it "parses an array" $
      basicArray `parseMatch` replicate 4 goodBasic
  describe "string parsing" $ do
    it "parses with an escaped backslash" $ do
      "\"foo\\\\\"" `parseMatch` ("foo\\" :: Text)
    it "parses with an escaped quote" $ do
      "\"foo\\\"\"" `parseMatch` ("foo\"" :: Text)
    it "parses with unicode" $ do
      "\"foo\\u2795\"" `parseMatch` ("foo➕" :: Text)
    it "parses a text with no escapes" $ do
      [r|"foo"|] `parseMatch` ("foo" :: Text)
    it "parses a text with an escaped backslash" $ do
      [r|"foo\\\\"|] `parseMatch` ([r|foo\\|] :: Text)
  describe "parsing a two-field object" $ do
    it "parses with only required fields" $
      twoSimple `parseMatch` goodTwo
    it "parses with only required fields in wrong order" $
      twoScramble `parseMatch` goodTwo
    it "parses with lots of extra crap" $
      twoExtra `parseMatch` goodTwo
    it "parses in a weird order with extra crap" $
      twoScrambleExtra `parseMatch` goodTwo
  describe "parsing a generically-derived object" $ do
    it "parses correctly in the basic case" $
      genericDefault `parseMatch` GenericStruct () [()]
  describe "parsing a generally-derived sum object" $ do
    it "parses first correctly" $
      sumBasic `parseMatch` GenericBasic (BasicStruct ())
    it "parses first correctrly when using other alternate" $
      sumBasicObj `parseMatch` GenericBasic (BasicStruct ())
    it "parses second correctly" $
      sumTwo `parseMatch` GenericTwo (TwoFields () ())
