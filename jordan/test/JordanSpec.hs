module Main
    ( main
    ) where

import qualified Jordan.FromJSON.AttoparsecSpec as APS
import qualified Jordan.FromJSON.MegaparsecSpec as MPS
import qualified Jordan.ToJSON.BuilderSpec as BS
import qualified Jordan.ToJSON.TextSpec as TS
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  MPS.spec
  APS.spec
  TS.spec
  BS.spec
