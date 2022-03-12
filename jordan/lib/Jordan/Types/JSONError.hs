{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Types.JSONError where

import Control.DeepSeq
import Data.Coerce
import Data.Foldable
import Data.Functor.Contravariant
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text.Lazy.Builder as LB
import GHC.Exts (IsList (..))
import GHC.Generics
import Jordan.FromJSON.Class
import Jordan.ToJSON.Class
import Jordan.Types.Internal.MergeMap
import Jordan.Types.JSONType

data JSONError
  = -- | Generic, user-provided error message
    ErrorMesage Text
  | -- | JSON was not up to spec
    ErrorInvalidJSON
  | -- | Bad type encountered (Expected, Actual)
    ErrorBadType {expectedType :: !JSONType, actualType :: !JSONType}
  | -- | There was no value for this JSON
    ErrorNoValue
  | -- | Text constant was wrong.
    ErrorBadTextConstant {expectedText :: !Text, actualText :: !Text}
  | -- | An object had some bad values.
    ErrorBadObject JSONObjectError
  | -- | An array had some bad indices
    ErrorBadArray JSONArrayError
  | -- | One of multiple possible errors.
    ErrorChoice !(Set.Set JSONError)
  deriving (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup JSONError where
  (<>) ErrorNoValue ErrorNoValue = ErrorNoValue
  (<>) ErrorNoValue !a = a
  (<>) !a ErrorNoValue = a
  (<>) (ErrorChoice lhs) (ErrorChoice rhs) = ErrorChoice $ lhs <> rhs
  (<>) (ErrorChoice a) !rhs = ErrorChoice $ a <> Set.singleton rhs
  (<>) !a (ErrorChoice rhs) = ErrorChoice (Set.singleton a <> rhs)
  (<>) !lhs !rhs = ErrorChoice (Set.singleton lhs <> Set.singleton rhs)

instance NFData JSONError

-- | 'mempty' is 'ErrorNoValue'
instance Monoid JSONError where
  mempty = ErrorNoValue

newtype JSONObjectError = MkJSONObjectError (Map.Map Text JSONError)
  deriving (Eq, Ord, Generic)
  deriving (NFData, IsList, Show, Read) via (Map.Map Text JSONError)
  deriving (Semigroup, Monoid) via (MergeMap Text JSONError)

instance FromJSON JSONObjectError where
  fromJSON = MkJSONObjectError <$> fromJSON

instance ToJSON JSONObjectError where
  toJSON = contramap (Map.toAscList . keyValueErrors) $ serializeDictionary toJSON

singleObjectError :: Text -> JSONError -> JSONObjectError
singleObjectError t = MkJSONObjectError . Map.singleton t

keyValueErrors :: JSONObjectError -> Map.Map Text JSONError
keyValueErrors = coerce

newtype JSONArrayError = MkJSONArrayError (Map.Map Integer JSONError)
  deriving (Eq, Ord, Generic)
  deriving (NFData, IsList, Show, Read) via (Map.Map Integer JSONError)
  deriving (Semigroup, Monoid) via (MergeMap Integer JSONError)

indexErrors :: JSONArrayError -> Map.Map Integer JSONError
indexErrors = coerce

instance FromJSON JSONArrayError where
  fromJSON = MkJSONArrayError <$> fromJSON

instance ToJSON JSONArrayError where
  toJSON = contramap indexErrors toJSON

prettyPrintJSONError :: JSONError -> Text
prettyPrintJSONError = go id
  where
    go :: (Text -> Text) -> JSONError -> Text
    go mapper = \case
      ErrorMesage txt -> mapper txt
      ErrorInvalidJSON -> mapper "Invalid JSON"
      ErrorBadType jt jt' -> mapper "Bad type: Expected " <> pack (show jt) <> " got " <> pack (show jt')
      ErrorNoValue -> mapper "Expected to receive a value, but did not"
      ErrorBadTextConstant txt txt' -> mapper "Bad text constant: Expected \"" <> txt <> "\", got \"" <> txt' <> "\""
      ErrorBadObject (MkJSONObjectError map) -> Map.foldlWithKey (\acc key value -> acc <> "\n" <> mapper key <> ":\n" <> go (mapper . ("  " <>)) value) mempty map
      ErrorBadArray (MkJSONArrayError map) -> Map.foldlWithKey (\acc key value -> acc <> "\n" <> mapper (pack $ show key) <> ":\n" <> go (mapper . ("  " <>)) value) mempty map
      ErrorChoice ne -> mapper "One of:\n" <> foldl' (\acc err -> acc <> go newMap err) mempty ne
        where
          newMap = mapper . ("  " <>)
