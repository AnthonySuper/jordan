module Jordan.ToJSON.BuilderSpec
    ( spec
    ) where

import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Jordan.SpecDefs (basicWritingSpec)
import Jordan.ToJSON.Builder (toJSONViaBuilder)
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Jordan.ToJSON.Builder" $ do
  basicWritingSpec $ decodeUtf8 . toStrict . toJSONViaBuilder
