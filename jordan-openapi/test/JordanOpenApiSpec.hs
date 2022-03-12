{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict.InsOrd as I
import Data.OpenApi
import Data.OpenApi.Declare (Declare, evalDeclare, execDeclare)
import Data.OpenApi.Internal
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Jordan (FromJSON (..), ToJSON (..))
import Jordan.OpenAPI (JordanToJSONSchema, getFromNamed, getToNamed)
import Jordan.OpenAPI.SpecDefs
import Optics.Operators ((^.))
import Test.Hspec (Expectation, SpecWith, before, describe, hspec, it, shouldBe, shouldContain)

main = do
  hspec $ do
    fileScenarios
    entryScenarios
    recursiveScenarios

shouldMakeNamedTo ::
  (ToJSON a) =>
  Proxy a ->
  Text ->
  Expectation
shouldMakeNamedTo p@Proxy name =
  (evalDeclare (getToNamed p) mempty ^. #_namedSchemaName) `shouldBe` Just name

shouldDeclareTo ::
  (ToJSON a) =>
  Proxy a ->
  Text ->
  Expectation
shouldDeclareTo p@Proxy name =
  I.keys (execDeclare (getToNamed p) mempty) `shouldContain` [name]

fileScenarios = describe "basic scenarios with file-like types" $ do
  it "works with file" $ do
    (Proxy @File) `shouldMakeNamedTo` "Jordan.OpenAPI.SpecDefs.File"
  it "works with directory" $ do
    (Proxy @Directory) `shouldMakeNamedTo` "Jordan.OpenAPI.SpecDefs.Directory"

entryScenarios = describe "bsaic scenarios with a directory-entry-like type" $ do
  it "includes definition for a file" $ do
    (Proxy @FileEntry) `shouldDeclareTo` "Jordan.OpenAPI.SpecDefs.File"
  it "includes definition for a directory" $ do
    (Proxy @FileEntry) `shouldDeclareTo` "Jordan.OpenAPI.SpecDefs.Directory"

recursiveScenarios = describe "scenarios with a recursive data type" $ do
  it "contains the top-level type" $ do
    (Proxy @Managership) `shouldDeclareTo` "Jordan.OpenAPI.SpecDefs.Managership"
  it "contains a nested type" $ do
    (Proxy @Managership) `shouldDeclareTo` "Jordan.OpenAPI.SpecDefs.Person"
