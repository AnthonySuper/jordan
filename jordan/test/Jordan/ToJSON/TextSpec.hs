{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Jordan.ToJSON.TextSpec
    where

import Data.Text (pack)
import GHC.Generics
import Jordan.SpecDefs (basicWritingSpec)
import Jordan.ToJSON.Class
import Jordan.ToJSON.Text
import Test.Hspec
import Text.RawString.QQ

shouldRenderJSON
  :: (ToJSON a, Show a, HasCallStack)
  => a
  -> String
  -> Expectation
shouldRenderJSON f a =
  runJSONText toJSON f "" `shouldBe` pack a

data UselessTuple
  = UselessTuple
  { first :: ()
  , second :: ()
  } deriving (Show, Generic)

instance ToJSON UselessTuple

newtype NestedTuple
  = NestedTuple
  { getNestedTuple :: UselessTuple }
  deriving (Show, Generic)

instance ToJSON NestedTuple

newtype WrapFoo
  = WrapFoo { getFoo :: () }
  deriving (Show, Generic)

instance ToJSON WrapFoo

newtype WrapBar
  = WrapBar { getBar :: () }
  deriving (Show, Generic)

instance ToJSON WrapBar

data PickOne
  = PickFoo WrapFoo
  | PickBar WrapBar
  deriving (Show, Generic)

instance ToJSON PickOne

spec :: Spec
spec = describe "Jordan.ToJSON.Text" $ do
  basicWritingSpec $ \v -> toJSONText v
  arrayRendering
  genericsRendering

arrayRendering :: Spec
arrayRendering = describe "array rendering" $ do
  it "renders an empty array properly" $ do
    ([] :: [()]) `shouldRenderJSON` "[]"
  it "renders a one-item array properly" $ do
    [()] `shouldRenderJSON` "[null]"
  it "renders a two-item array properly" $ do
    [(), ()] `shouldRenderJSON` "[null,null]"

genericsRendering :: Spec
genericsRendering = describe "generics rendering" $ do
  it "can render a basic two-field object" $ do
    UselessTuple () () `shouldRenderJSON` [r|{"first": null, "second": null}|]
  it "can render a basic two-field object nested" $ do
    NestedTuple (UselessTuple () ()) `shouldRenderJSON`
      [r|{"getNestedTuple": {"first": null, "second": null}}|]
  it "can render first case of PickOne" $ do
    PickFoo (WrapFoo ()) `shouldRenderJSON`
      [r|{"PickFoo": {"getFoo": null}}|]
  it "can render second case of pickone" $ do
    PickBar (WrapBar ()) `shouldRenderJSON`
      [r|{"PickBar": {"getBar": null}}|]
