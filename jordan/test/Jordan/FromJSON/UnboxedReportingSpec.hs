{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Jordan.FromJSON.UnboxedReportingSpec
  ( spec,
    prettyWhenError,
  )
where

import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Jordan
import Jordan.FromJSON.Class (FromJSON (fromJSON))
import Jordan.SpecDefs (basicParsingSpec)
import Jordan.Types.JSONError
import Jordan.Types.JSONType
import Test.Hspec (Spec, describe, fdescribe, fit, it, shouldBe)

data Age
  = RealAge Int
  | PreferNotToSay
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON Age where
  toJSON = select selectAge toJSON (serializeTextConstant "PreferNotToSay")
    where
      selectAge = \case
        RealAge i -> Left i
        PreferNotToSay -> Right ()

instance FromJSON Age where
  fromJSON = (RealAge <$> fromJSON) <> (parseTextConstant "PreferNotToSay" $> PreferNotToSay)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Age
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ShortPerson = ShortPerson {getShortPerson :: Person}
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON ShortPerson where
  fromJSON =
    (ShortPerson <$> fromJSON)
      <> ( ShortPerson
             <$> parseObject
               ( Person
                   <$> parseField "f"
                   <*> parseField "l"
                   <*> parseField "a"
               )
         )

-- age key, have error
-- can keep going
-- try to keep going

errorTests = describe "parse errors" $ do
  let p = parseOrReport @Person
  let p' = parseOrReport @ShortPerson
  let noValuesP = Left (ErrorBadObject [("firstName", ErrorNoValue), ("lastName", ErrorNoValue), ("age", ErrorNoValue)])
  it "does the right thing for an empty object" $
    p "{}"
      `shouldBe` noValuesP
  it "does the right thing for a junk object" $
    p "{ \"foo\": null }"
      `shouldBe` noValuesP
  it "does the right thing with one key provided" $
    p "{ \"firstName\": \"Bob\" }"
      `shouldBe` Left (ErrorBadObject [("lastName", ErrorNoValue), ("age", ErrorNoValue)])
  it "does the right thing with two keys provided" $
    p "{ \"age\": 10, \"firstName\": \"rich\" }"
      `shouldBe` Left (ErrorBadObject [("lastName", ErrorNoValue)])
  let nullAge = Left (ErrorBadObject [("age", ErrorChoice [ErrorBadType JSONTypeNumber JSONTypeNull, ErrorBadType JSONTypeText JSONTypeNull])])
  it "does the right thing where the last key has a bad type" $
    p "{ \"firstName\": \"Bob\", \"lastName\": \"Smith\", \"age\": null }"
      `shouldBe` nullAge
  it "does the right thing when the last key has a bad type and there is a junk key" $
    p "{ \"firstName\": \"Bob\", \"lastName\": \"Smith\", \"age\": null, \"junk\": true }"
      `shouldBe` nullAge
  it "does the right thing where the first key has a bad type" $
    p "{ \"age\": null, \"firstName\": \"Bob\", \"lastName\": \"Smith\" }"
      `shouldBe` Left (ErrorBadObject [("age", ErrorChoice [ErrorBadType JSONTypeNumber JSONTypeNull, ErrorBadType JSONTypeText JSONTypeNull])])
  it "can express multiple object errors" $
    p' "{ \"firstName\": \"Rich\", \"f\": \"rich\" }"
      `shouldBe` Left
        ( ErrorChoice
            [ ErrorBadObject
                [("age", ErrorNoValue), ("lastName", ErrorNoValue)],
              ErrorBadObject
                [("a", ErrorNoValue), ("l", ErrorNoValue)]
            ]
        )

prettyWhenError :: Either JSONError a -> Either Text a
prettyWhenError = \case
  Left je -> Left $ prettyPrintJSONError je
  Right a -> Right a

spec :: Spec
spec = describe "Jordan.FromJSON.Reporting" $ do
  errorTests
  basicParsingSpec $ \t v ->
    prettyWhenError (parseOrReport (encodeUtf8 t)) `shouldBe` Right v
