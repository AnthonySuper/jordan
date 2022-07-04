{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus (mzero), guard)
import Criterion.Main
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.Proxy (Proxy (..))
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified Jordan as J
import Jordan.Benchmark.GeoJSON (FeatureCollection)
import Jordan.Benchmark.OnlyWanted (OnlyWanted)
import Jordan.Types.JSONError (JSONError, prettyPrintJSONError)

data BenchmarkFiles = BenchmarkFiles
  { usStates :: B.ByteString,
    tooManyKeys :: B.ByteString
  }
  deriving (Show, Generic)

instance NFData BenchmarkFiles

readBenchmarkFiles :: IO BenchmarkFiles
readBenchmarkFiles =
  BenchmarkFiles
    <$> B.readFile "data/us-states.json"
    <*> B.readFile "data/too-many-keys.json"

benchAtto (Proxy :: Proxy p) strategy bs = do
  guard True
  [bench "Jordan Attoparsec (nf)" $ strategy (\f -> J.parseViaAttoparsecWith J.fromJSON f :: Either String p) bs]

benchRep (Proxy :: Proxy p) strategy bs = do
  guard True
  [bench "Jordan Reporting (nf)" $ strategy (\f -> J.parseOrReportWith J.fromJSON f :: Either JSONError p) bs]

benchAeson (Proxy :: Proxy p) strategy bs = do
  guard True
  [bench "Aeson (nf)" $ strategy (\f -> A.decode f :: Maybe p) (fromStrict bs)]

makeBenchBoth ::
  (A.FromJSON p, J.FromJSON p, NFData p) =>
  String ->
  String ->
  Proxy p ->
  Benchmark
makeBenchBoth label path p@(Proxy :: Proxy p) =
  env (B.readFile path) $ \bs ->
    bgroup
      label
      $ benchAtto p nf bs
        ++ benchAeson p nf bs
        ++ benchRep p nf bs

makeBench ::
  (A.FromJSON p, J.FromJSON p, NFData p) =>
  String ->
  String ->
  Proxy p ->
  [Benchmark]
makeBench label fname p = [makeBenchBoth label fname p]

main :: IO ()
main = do
  defaultMain $
    makeBench "GeoJSON of US States" "data/us-states.json" (Proxy :: Proxy FeatureCollection)
      <> makeBench "Object field with lots of extra keys" "data/too-many-keys.json" (Proxy :: Proxy OnlyWanted)

