{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jordan.Benchmark.GeoJSON
    where

import Control.DeepSeq
import GHC.Generics
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Jordan as J

data Point
  = Point { x :: !Double, y :: !Double }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (NFData)

instance J.FromJSON Point where
  fromJSON = J.parseTuple $
    Point <$> J.consumeItem <*> J.consumeItem

instance A.FromJSON Point where
  parseJSON o = do
    x <- A.parseJSON o
    case x of
      [x, y] -> pure $ Point x y
      _ -> fail "bad point"

newtype LineString = LineString { getLineString :: [Point] }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass NFData

instance J.FromJSON LineString where
  fromJSON = LineString <$> J.parseArray

instance A.FromJSON LineString where
  parseJSON o = LineString <$> A.parseJSON o

data Polygon
  = Polygon
  { boundary :: LineString
  , hole :: Maybe LineString
  } deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass NFData

instance J.FromJSON Polygon where
  fromJSON = J.parseObject "GeoJSON.Polygon"
    (J.parseFieldWith "type" (J.parseTextConstant "Polygon") *>
      J.parseFieldWith "coordinates" (nohole <> hole))
    where
      nohole = J.parseTuple
        (Polygon <$> J.consumeItem <*> pure Nothing)
      hole = J.parseTuple
        (Polygon <$> J.consumeItem <*> (Just <$> J.consumeItem))

instance A.FromJSON Polygon where
  parseJSON (A.Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "Polygon" -> do
        geom <- o .: "coordinates"
        case geom of
          [x] -> pure $ Polygon x Nothing
          [x, h] -> pure $ Polygon x (Just h)
          _ -> fail "bad parse"
      _ -> fail "bad type"
  parseJSON _ = fail "not an object"


newtype Feature
  = Feature
  { geometry :: Polygon }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass NFData

instance J.FromJSON Feature where
  fromJSON
    = J.parseObject "GeoJSON.Feature"
    $ J.parseFieldWith "type" (J.parseTextConstant "Feature")
    *> (Feature <$> J.parseField "geometry")

instance A.FromJSON Feature where
  parseJSON (A.Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "Feature" -> Feature <$> o .: "geometry"
      _ -> fail "bad type"
  parseJSON _ = fail "not an object"

newtype FeatureCollection
  = FeatureCollection { features :: [Feature] }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass NFData

instance J.FromJSON FeatureCollection where
  fromJSON
    = J.parseObject "GeoJSON.FeatureCollection"
    $ J.parseFieldWith "type" (J.parseTextConstant "FeatureCollection")
    *> (FeatureCollection <$> J.parseField "features")

instance A.FromJSON FeatureCollection where
  parseJSON (A.Object o) = do
    (r :: Text) <- o .: "type"
    case r of
      "FatureCollection" -> FeatureCollection <$> o .: "features"
      _ -> fail "wrong type"
  parseJSON _ = fail "not an object"
