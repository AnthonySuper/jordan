{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    where

import Control.DeepSeq (NFData)
import Criterion.Main
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import qualified Jordan as J
import Jordan.Benchmark.GeoJSON (FeatureCollection)
import Jordan.Benchmark.OnlyWanted (OnlyWanted)

data BenchmarkFiles
  = BenchmarkFiles
  { usStates :: B.ByteString
  , tooManyKeys :: B.ByteString
  } deriving (Show, Generic)

instance NFData BenchmarkFiles

readBenchmarkFiles ::  IO BenchmarkFiles
readBenchmarkFiles
  = BenchmarkFiles
  <$> B.readFile "data/us-states.json"
  <*> B.readFile "data/too-many-keys.json"

makeBenchBoth
  :: (A.FromJSON p, J.FromJSON p, NFData p)
  => String
  -> String
  -> Proxy p
  -> Benchmark
makeBenchBoth label path (Proxy :: Proxy p)
  = env (B.readFile path) $ \bs -> bgroup label
  [ bench "jordan (normal form)" $ nf (\f -> J.runParserViaAttoparsec J.fromJSON f :: Either String p) bs
  , bench "aeson (normal form)" $ nf (\f -> A.decode f :: Maybe p) (fromStrict bs)
  ]

makeBench
  :: (A.FromJSON p, J.FromJSON p, NFData p)
  => String
  -> String
  -> Proxy p
  -> [Benchmark]
makeBench label fname p = [makeBenchBoth label fname p]

main :: IO ()
main
  = defaultMain
  $ makeBench "GeoJSON of US States" "data/us-states.json" (Proxy :: Proxy FeatureCollection)
  <> makeBench "Object field with lots of extra keys" "data/too-many-keys.json" (Proxy :: Proxy OnlyWanted)
