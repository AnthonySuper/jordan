{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Jordan.Servant.Query.RoundtripSpec where

import Control.Applicative
import Control.Monad (guard)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Coerce
import Data.Functor.Contravariant
import Data.Proxy
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import Jordan
import Jordan.Servant.Query.Parse (parseQueryAtKey, parseQueryToKeys, transformToKey, unbracedValue)
import Jordan.Servant.Query.Render (escapeRawComponent, renderQueryAtKey)
import Network.HTTP.Types.URI
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Utf8

data Color = Red | Green | Blue | Black | Orange | Yellow | White
  deriving (Show, Read, Eq, Ord, Generic, Bounded, Enum)
  deriving anyclass (ToJSON, FromJSON)

data HomogenousCoord = HomogenousCoord {x :: Int, y :: Int, z :: Int}
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON HomogenousCoord where
  fromJSON =
    parseObject
      ( HomogenousCoord
          <$> parseField "x"
          <*> parseField "y"
          <*> parseFieldWithDefault "z" fromJSON 0
      )

instance ToJSON HomogenousCoord where
  toJSON =
    serializeObject $
      divide
        (\(HomogenousCoord x y z) -> (x, (y, z)))
        (serializeField "x")
        $ divide
          id
          (serializeField "y")
          $ contramap
            (\x -> if x == 0 then Nothing else Just x)
            (serializeJust "z" toJSON)

instance Arbitrary HomogenousCoord where
  arbitrary =
    HomogenousCoord
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (HomogenousCoord x y z) =
    tail $ HomogenousCoord <$> shrink x <*> shrink y <*> shrink z

newtype Coords = Coords {getCoords :: [HomogenousCoord]}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving (Arbitrary) via [HomogenousCoord]

deriving via [HomogenousCoord] instance (FromJSON Coords)

deriving via [HomogenousCoord] instance (ToJSON Coords)

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

shrinkText :: T.Text -> [T.Text]
shrinkText t
  | T.length t == 0 = []
  | T.length t == 1 = [mempty]
  | otherwise = mempty : (T.pack . pure <$> T.unpack t)

data Person = Person
  { firstName :: T.Text,
    lastName :: T.Text,
    age :: Int
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary Person where
  arbitrary = Person <$> genValidUtf8 <*> genValidUtf8 <*> arbitrary
  shrink (Person fn ln a) = tail $ Person <$> shrinkText fn <*> shrinkText ln <*> shrink a

data WeirdEnum
  = WeirdNothing
  | WeirdDuo Person Person
  | WeirdSingle Person
  | WeirdColor Color
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary WeirdEnum where
  arbitrary =
    oneof
      [ pure WeirdNothing,
        WeirdDuo <$> arbitrary <*> arbitrary,
        WeirdSingle <$> arbitrary,
        WeirdColor <$> arbitrary
      ]
  shrink (WeirdDuo lhs rhs) =
    tail (WeirdDuo <$> shrink lhs <*> shrink rhs)
      <|> tail (WeirdDuo lhs <$> shrink rhs)
      <|> tail (WeirdDuo <$> shrink lhs <*> pure rhs)
  shrink (WeirdSingle a) = tail $ WeirdSingle <$> shrink a
  shrink _ = []

newtype Feedback = Feedback {ratings :: [(T.Text, Int)]}
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Feedback where
  fromJSON = Feedback <$> parseDictionary fromJSON

instance ToJSON Feedback where
  toJSON = contramap ratings $ serializeDictionary toJSON

instance Arbitrary Feedback where
  arbitrary = Feedback <$> liftArbitrary ((,) <$> genValidUtf8 <*> arbitrary)
  shrink (Feedback xs) = Feedback <$> liftShrink shrinkPair xs
    where
      shrinkPair (l, k) = tail $ do
        l' <- shrinkText l
        k' <- shrink k
        pure (l', k')

data PersonFeedback = PersonFeedback {feedbackGiver :: Person, feedbackGiven :: Feedback}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary PersonFeedback where
  arbitrary = PersonFeedback <$> arbitrary <*> arbitrary
  shrink (PersonFeedback p f) = PersonFeedback <$> shrink p <*> shrink f

data CoverBases
  = CoverNest PersonFeedback
  | CoverNull ()
  | CoverArray [PersonFeedback]
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary CoverBases where
  arbitrary =
    oneof
      [ CoverNest <$> arbitrary,
        pure (CoverNull ()),
        CoverArray <$> arbitrary
      ]
  shrink = \case
    CoverNest pf -> CoverNest <$> shrink pf
    CoverNull x0 -> []
    CoverArray xs -> CoverArray <$> shrink xs

robustShrinkWith :: (a -> [a]) -> [a] -> [[a]]
robustShrinkWith _ [] = []
robustShrinkWith shrink (x : xs) =
  robustShrinkWith shrink xs
    ++ [x : xs' | xs' <- robustShrinkWith shrink xs]
    ++ [x' : xs | x' <- shrink x]
    ++ [shrink x]

newtype QueryKey = QueryKey {getQueryKey :: T.Text}
  deriving (Show, Read, Eq, Ord, Generic)

instance Arbitrary QueryKey where
  arbitrary =
    QueryKey
      <$> genValidUtf8 `suchThat` (\t -> T.length t /= 0)
  shrink (QueryKey k)
    | T.length k < 2 = []
    | otherwise = QueryKey . T.pack . pure <$> T.unpack k

queryEquals :: (ToJSON a, FromJSON a, Show a, Arbitrary a, Eq a) => Proxy a -> Property
queryEquals (Proxy :: Proxy a) = forAll arbitrary cb
  where
    cb :: (QueryKey, a) -> Property
    cb (key, v) =
      let key' = getQueryKey key
          renderedKey = urlEncode True (encodeUtf8 key')
          unrenderedKey = decodeUtf8 $ urlDecode True renderedKey
          rendered = renderQuery False $ renderQueryAtKey key' v
          parsedBS = parseQuery rendered
          transformed = transformToKey key' parsedBS
          toKeys = parseQueryToKeys parsedBS
          parsed = parseQueryAtKey @a key' $ parseQuery rendered
       in counterexample
            ("Raw was " <> show rendered)
            (parsed === Right v)

spec :: Spec
spec = do
  it "round-trips the base key" $
    property $
      forAll arbitrary $ \k ->
        let key = getQueryKey k
            escaped = escapeRawComponent key
         in counterexample ("Escaped was " <> show escaped) $
              parseOnly unbracedValue escaped === Right key
  describe "round-tripping" $ do
    it "works for enums" $ queryEquals (Proxy @Color)
    it "works for structs" $ queryEquals (Proxy @Person)
    it "works for weird stuff" $ queryEquals (Proxy @WeirdEnum)
    it "works for dictionaries" $ queryEquals (Proxy @Feedback)
    it "works for person feedback" $ queryEquals (Proxy @PersonFeedback)
    fit "works for a weird sum type crap" $ queryEquals (Proxy @CoverBases)
