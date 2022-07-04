{-# LANGUAGE LambdaCase #-}

module Jordan.FromJSON.UnboxedReportingSpec
  ( spec,
    prettyWhenError,
  )
where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Jordan
import Jordan.FromJSON.Class (FromJSON (fromJSON))
import Jordan.SpecDefs (basicParsingSpec)
import Jordan.Types.JSONError
import Test.Hspec (Spec, describe, it, shouldBe)

prettyWhenError :: Either JSONError a -> Either Text a
prettyWhenError = \case
  Left je -> Left $ prettyPrintJSONError je
  Right a -> Right a

spec :: Spec
spec = describe "Jordan.FromJSON.Reporting" $ do
  basicParsingSpec $ \t v ->
    prettyWhenError (parseOrReport (encodeUtf8 t)) `shouldBe` Right v
