module Jordan.FromJSON.AttoparsecSpec
    ( spec
    ) where

import Data.Text.Encoding (encodeUtf8)
import Jordan.FromJSON.Attoparsec
import Jordan.FromJSON.Class (FromJSON)
import Jordan.SpecDefs (basicParsingSpec)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Jordan.FromJSON.Attoparsec" $ do
  basicParsingSpec $ \t v ->
    parseViaAttoparsec (encodeUtf8 t) `shouldBe` Right v
