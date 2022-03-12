module Main
  ( main,
  )
where

import qualified Jordan.FromJSON.AttoparsecSpec as APS
import qualified Jordan.FromJSON.UnboxedReportingSpec as UR
import qualified Jordan.RoundTripSpec as RTS
import qualified Jordan.ToJSON.BuilderSpec as BS
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  APS.spec
  BS.spec
  RTS.spec
  UR.spec
