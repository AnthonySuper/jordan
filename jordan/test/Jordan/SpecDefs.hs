{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Jordan.SpecDefs where

import Control.Applicative (Alternative ((<|>)))
import Data.Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Scientific (Scientific, scientific)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Jordan.FromJSON.Class
import Jordan.Generic.Options
import Jordan.ToJSON.Class
import Test.Hspec (Arg, Example, Spec, SpecWith, describe, fit, it, shouldBe)
import Text.RawString.QQ

newtype BasicStruct = BasicStruct {bar :: ()}
  deriving (Show, Eq, Ord)

goodBasic :: BasicStruct
goodBasic = BasicStruct ()

instance FromJSON BasicStruct where
  fromJSON = (BasicStruct <$> fromNull) <> fromObject
    where
      fromObject =
        parseObject $
          BasicStruct <$> parseField "bar"
      fromNull = parseNull

instance ToJSON BasicStruct where
  toJSON = serializeObject $ serializeFieldWith "bar" serializeNull

basicNull = pack [r| null |]

basicOneFieldNoSpace = pack [r|{"bar":null}|]

basicOneField = pack [r| { "bar": null } |]

basicEscaped = pack [r| { "\u0062\u0061\u0072": null }|]

basicEscapedNoSpace = pack [r|{"\u0062\u0061\u0072":null}|]

basicPartialEscape = pack [r| { "\u0062a\u0072": null } |]

basicExtraFields = pack [r| { "whatever": null, "bar": null, "baz": null } |]

basicArray = pack [r| [null, { "whatever": null, "bar": null }, null, null] |]

data TwoFields = TwoFields {one :: (), two :: ()}
  deriving (Show, Eq, Ord, Generic)

goodTwo :: TwoFields
goodTwo = TwoFields () ()

instance FromJSON TwoFields where
  fromJSON = parseObject $ TwoFields <$> parseField "one" <*> parseField "two"

twoSimple = pack [r| { "one"   :   null, "two": null } |]

twoScramble = pack [r| {"two":null,"one":null} |]

twoExtra =
  pack
    [r|
  {
    "ignore": [],
    "one": null,
    "four": [],
    "five": "test",
    "seven": {},
    "six": "why tho",
    "two": null,
    "three": {},
    "ignored": [1,2,3,4, { " foo " : null }]
  }
|]

twoScrambleExtra =
  pack
    [r|
  {
    "ignore": null,
    "bad": null,
    "two": null,
    "another": null,
    "yetAgain": null,
    "one": null
  }
|]

data GenericStruct = GenericStruct
  { firstLabel :: (),
    secondLabel :: [()]
  }
  deriving (Show, Read, Eq, Ord, Generic)

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

data Coord = Coord {x :: !Double, y :: !Double}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON Coord where
  fromJSON = fromObject <> fromArray
    where
      fromObject = parseObject $ Coord <$> parseField "x" <*> parseField "y"
      fromArray = parseTuple $ Coord <$> consumeItem <*> consumeItem

coordBasic = pack [r|{ "x": 10, "y": 10 } |]

coordReverse = pack [r| { "y": 11, "x": 12 } |]

coordExtra = pack [r| { "x": 1, "y": 2, "z": 3 } |]

coordNoSpaceDecimal = pack [r|{"x":0.0,"y":0.0}|]

coordTuple = pack [r| [1, 20] |]

data HomogenousCoord = HomogenousCoord {hx :: !Double, hy :: !Double, hz :: !Double}
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON HomogenousCoord where
  fromJSON = fromObject <> fromObjectZero <> fromArray <> fromArrayZero
    where
      fromObject = parseObject $ mk <$> parseField "x" <*> parseField "y" <*> parseField "z"
      fromObjectZero = parseObject $ mk <$> parseField "x" <*> parseField "y" <*> pure 1.0
      fromArray = parseTuple $ mk <$> consumeItem <*> consumeItem <*> consumeItem
      fromArrayZero = parseTuple $ mk <$> consumeItem <*> consumeItem <*> pure 1.0
      mk = HomogenousCoord

data WeirdFeedback
  = GeneralFeedback Text
  | SpecificFeedback [(Text, Text)]
  deriving (Show, Eq)

instance FromJSON WeirdFeedback where
  fromJSON =
    parseObject (GeneralFeedback <$> parseField "general")
      <> (SpecificFeedback <$> parseDictionary fromJSON)

basicWritingSpec ::
  (forall val. (ToJSON val, Show val) => val -> Text) ->
  Spec
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
      writeJSON [(), ()] `shouldBe` "[null,null]"
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

basicParsingSpec ::
  (Example a) =>
  (forall val. (FromJSON val, Show val, Eq val) => Text -> val -> a) ->
  SpecWith (Arg a)
basicParsingSpec parseMatch = do
  let parseExample t v = it ("parses " <> show t <> " to " <> show v) $ t `parseMatch` v

  describe "parsing basic structure" $ do
    parseExample "true" True
    parseExample "false" False
    parseExample "null" ()
  describe "parsing numbers" $ do
    let m (t, e, i) = parseExample t $ scientific e i
    traverse_
      m
      [ ("0", 0, 0),
        ("1", 1, 0),
        ("20", 20, 0),
        ("190865", 190865, 0),
        ("-0", 0, 0),
        ("-1", -1, 0),
        ("-10.5", -105, -1),
        ("-1e100", -1, 100)
      ]
  describe "basic array parsing" $ do
    "[]" `parseExample` ([] :: [()])
    "[null ]" `parseExample` [()]
    "[null\n,null]" `parseExample` [(), ()]
    basicNull `parseExample` goodBasic
    basicArray `parseExample` replicate 4 goodBasic
  describe "basic object parsing" $ do
    basicOneFieldNoSpace `parseExample` goodBasic
    basicOneField `parseExample` goodBasic
    basicExtraFields `parseExample` goodBasic
    basicEscaped `parseExample` goodBasic
    basicEscapedNoSpace `parseExample` goodBasic
    basicPartialEscape `parseExample` goodBasic
  describe "parsing coordinates" $ do
    coordBasic `parseExample` Coord 10 10
    coordReverse `parseExample` Coord 12 11
    coordExtra `parseExample` Coord 1 2
    coordTuple `parseExample` Coord 1 20
    coordNoSpaceDecimal `parseExample` Coord 0 0
  describe "parsing homogenous coords" $ do
    "[1,2]" `parseExample` HomogenousCoord 1 2 1
    "[1,2,2]" `parseExample` HomogenousCoord 1 2 2
    "{ \"x\": 1, \"y\": 2, \"z\": 10 }" `parseExample` HomogenousCoord 1 2 10
  describe "string parsing" $ do
    "\"foo\\\\\"" `parseExample` ("foo\\" :: Text)
    "\"foo\\\"\"" `parseExample` ("foo\"" :: Text)
    "\"foo\\u2795\"" `parseExample` ("foo➕" :: Text)
    [r|"foo"|] `parseExample` ("foo" :: Text)
    [r|"foo\\\\"|] `parseExample` ([r|foo\\|] :: Text)
  describe "parsing a two-field object" $ do
    twoSimple `parseExample` goodTwo
    twoScramble `parseExample` goodTwo
    twoExtra `parseExample` goodTwo
    twoScrambleExtra `parseExample` goodTwo
  describe "parsing a generically-derived object" $ do
    genericDefault `parseExample` GenericStruct () [()]
  describe "parsing a generally-derived sum object" $ do
    sumBasic `parseExample` GenericBasic (BasicStruct ())
    sumBasicObj `parseExample` GenericBasic (BasicStruct ())
    sumTwo `parseExample` GenericTwo (TwoFields () ())
  describe "object parsing with possible weirdness" $ do
    pack [r| {"general": "it sucked", "foo": "bar"} |] `parseExample` GeneralFeedback "it sucked"
    pack [r| {"foo": "bar"} |] `parseExample` SpecificFeedback [("foo", "bar")]
