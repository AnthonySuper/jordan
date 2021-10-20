{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Jordan.FromJSON.MegaparsecSpec
    ( spec
    ) where

import Data.Text (Text)
import Jordan.FromJSON.Class
import Jordan.FromJSON.Megaparsec
import Jordan.SpecDefs
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

parse' a = parse a ""

parseJ :: (FromJSON a) => Text -> Either ParseError a
parseJ = parse' megaparsecParser

spec :: Spec
spec = describe "megaparsec parsing" $ do
  commaSpec
  anyFieldSpec
  jsonTextSpec
  basicParsingSpec (\t v -> parseJ t `shouldParse` v)
  specialCaseSpec

commaSpec :: Spec
commaSpec = describe "comma parser" $ do
  it "parses just a comma" $
    parse' comma `shouldSucceedOn` ","
  it "parses a comma with some extra whitespace" $
    parse' comma `shouldSucceedOn` ",    \n\n"

anyFieldSpec :: Spec
anyFieldSpec = describe "parseAnyField" $ do
  it "parses a very basic null field" $
    parse' parseAnyField `shouldSucceedOn` "\"foo\": null"
  it "parses an overly whitespaced field" $
    parse' parseAnyField `shouldSucceedOn` "\"foo\"  : \n\n\n null"

jsonTextSpec :: Spec
jsonTextSpec = describe "parseJSONText" $ do
  let parseText = parse' parseJSONText
  it "parses a super basic string" $ do
    parseText `shouldSucceedOn` "\"foo\""
  it "parses with an escaped backslash" $ do
    parseText "\"foo\\\\\"" `shouldParse` "foo\\"
  it "parses with an escaped quote" $ do
    parseText "\"foo\\\"\"" `shouldParse` "foo\""
  it "parses with unicode" $ do
    parseText "\"foo\\u2795\"" `shouldParse` "foo➕"
  it "parses with trailing whitespace" $ do
    parseText "\"foo\"  " `shouldParse` "foo"

specialCaseSpec :: Spec
specialCaseSpec = describe "special cases" $ do
  it "does not allow mismatched labels" $
    parse (megaparsecParser @GenericSum) "" `shouldFailOn` mismatchType
