module Main
    where

import Criterion.Main
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict)
import qualified Jordan as J
import Jordan.Benchmark.GeoJSON

readStateJSON :: IO B.ByteString
readStateJSON = B.readFile "data/us-states.json"

main :: IO ()
main = defaultMain [
  env readStateJSON $ \file ->
    bgroup "GeoJSON" $
      [ bench "jordan" $ nf (\f -> J.runParserViaAttoparsec J.fromJSON f :: Either String FeatureCollection) file
      , bench "aeson" $ nf (\f -> A.decode (fromStrict f) :: Maybe FeatureCollection) file
      ]
  ]

