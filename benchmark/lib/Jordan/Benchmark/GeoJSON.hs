{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Jordan.Benchmark.GeoJSON where

import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics
import qualified Jordan as J

data Point = Point {x :: Double, y :: Double}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON Point where
  fromJSON =
    J.nameParser "GeoJSON.Point.Input" $
      J.parseTuple $
        Point <$> J.consumeItem <*> J.consumeItem
  {-# INLINE fromJSON #-}

instance A.FromJSON Point where
  parseJSON o = do
    x <- A.parseJSON o
    case x of
      [x, y] -> pure $ Point x y
      _ -> fail "bad point"

newtype LineString = LineString {getLineString :: [Point]}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON LineString where
  fromJSON = J.nameParser "GeoJSON.LineString.Input" $ LineString <$> J.parseArray
  {-# INLINE fromJSON #-}

instance A.FromJSON LineString where
  parseJSON o = LineString <$> A.parseJSON o
  {-# INLINE parseJSON #-}

data PolygonCoordinates = PolygonCoordinates
  { boundary :: LineString,
    holes :: [LineString]
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON PolygonCoordinates where
  fromJSON =
    J.nameParser "GeoJSON.Polygon.Coordinates" $
      J.validateJSON $
        mapCoords <$> J.fromJSON
    where
      mapCoords (x : xs) = Right (PolygonCoordinates x xs)
      mapCoords [] = Left "Illegal empty polygon array"

instance A.FromJSON PolygonCoordinates where
  parseJSON o = do
    res <- A.parseJSON o
    case res of
      (x : xs) -> pure $ PolygonCoordinates x xs
      _ -> fail "Not enough stuff"

newtype Polygon = Polygon
  { getPolygon :: PolygonCoordinates
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON Polygon where
  fromJSON =
    J.nameParser "GeoJSON.Polygon.Input" $
      J.parseObject
        ( J.parseFieldWith "type" (J.parseTextConstant "Polygon")
            *> J.parseFieldWith "coordinates" (Polygon <$> J.fromJSON)
        )
  {-# INLINE fromJSON #-}

instance A.FromJSON Polygon where
  parseJSON (A.Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "Polygon" -> Polygon <$> o .: "coordinates"
      _ -> fail "not a polygon"
  parseJSON _ = fail "not a polygon"

newtype MultiPolygon = MultiPolygon {polygons :: [Polygon]}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON MultiPolygon where
  fromJSON =
    J.parseObject $
      J.parseFieldWith "type" (J.parseTextConstant "MultiPolygon")
        *> J.parseFieldWith "coordinates" (MultiPolygon . map Polygon <$> J.fromJSON)

instance A.FromJSON MultiPolygon where
  parseJSON (A.Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "MultiPolygon" ->
        MultiPolygon . map Polygon <$> o .: "coordinates"
      _ -> fail "not a multipolygon"
  parseJSON _ = fail "not a multipolygon"

data Geometry
  = GPoint Point
  | GLineString LineString
  | GPolygon Polygon
  | GMultiPolygon MultiPolygon
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON Geometry where
  fromJSON =
    J.nameParser "GeoJSON.Geometry.Input" $
      (GPoint <$> J.fromJSON)
        <> (GLineString <$> J.fromJSON)
        <> (GPolygon <$> J.fromJSON)
        <> (GMultiPolygon <$> J.fromJSON)

instance A.FromJSON Geometry where
  parseJSON v =
    (GPoint <$> A.parseJSON v)
      <|> (GLineString <$> A.parseJSON v)
      <|> (GPolygon <$> A.parseJSON v)
      <|> (GMultiPolygon <$> A.parseJSON v)

newtype Feature = Feature
  {geometry :: Geometry}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON Feature where
  fromJSON =
    J.nameParser "GeoJSON.Feature.Input" $
      J.parseObject $
        J.parseFieldWith "type" (J.parseTextConstant "Feature")
          *> (Feature <$> J.parseField "geometry")
  {-# INLINE fromJSON #-}

instance A.FromJSON Feature where
  parseJSON (A.Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "Feature" -> Feature <$> o .: "geometry"
      _ -> fail "bad type"
  parseJSON _ = fail "not an object"

newtype FeatureCollection = FeatureCollection {features :: [Feature]}
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON FeatureCollection where
  fromJSON =
    J.nameParser "GeoJSON.FeatureCollection.Input" $
      J.parseObject $
        J.parseFieldWith "type" (J.parseTextConstant "FeatureCollection")
          *> (FeatureCollection <$> J.parseField "features")
  {-# INLINE fromJSON #-}

instance A.FromJSON FeatureCollection where
  parseJSON (A.Object o) = do
    (r :: Text) <- o .: "type"
    case r of
      "FeatureCollection" -> FeatureCollection <$> o .: "features"
      _ -> fail "wrong type"
  parseJSON _ = fail "not an object"
