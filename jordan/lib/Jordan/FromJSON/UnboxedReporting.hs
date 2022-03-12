{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Attempts to parse to either a result type or a direct report, by using a custom parser.
-- This parser uses Haskell primops to try to avoid allocations.
-- At the end of the day, it's not as fast as Attoparsec, but it's pretty dang fast.
--
-- We could not use Attoparsec directly due to the need for differnet error handling.
-- Other libraries with correct error handling behavior do exist, but in order to keep the dependency footprint low,
-- we rolled our own.
module Jordan.FromJSON.UnboxedReporting (parseOrReportWith, parseOrReport) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Applicative.Combinators (sepBy)
import Control.Monad (when)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe as BS
import Data.Char (chr, isControl, ord)
import Data.Functor (void, ($>))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Alt (..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import Jordan.FromJSON.Class
import Jordan.FromJSON.Internal.Attoparsec (bsToInteger)
import Jordan.FromJSON.Internal.Permutation
import Jordan.FromJSON.Internal.UnboxedParser as UP hiding (AccumE (..), AccumEL, AccumER)
import Jordan.FromJSON.Internal.UnboxedReporting
import Jordan.Types.Internal.AccumE (AccumE (AccumE))
import Jordan.Types.JSONError
  ( JSONArrayError (..),
    JSONError
      ( ErrorBadArray,
        ErrorBadObject,
        ErrorBadTextConstant,
        ErrorBadType,
        ErrorInvalidJSON,
        ErrorMesage,
        ErrorNoValue
      ),
    JSONObjectError (..),
  )
import Jordan.Types.JSONType (JSONType (..))
import Numeric (showHex)

newtype ReportingParser a = ReportingParser {runReportingParser :: UP.Parser JSONError a}
  deriving (Functor) via (UP.Parser JSONError)
  deriving (Semigroup) via (Alt (UP.Parser JSONError) a)

newtype ReportingObjectParser a = ReportingObjectParser
  {runReportingObjectParser :: Permutation (UP.Parser JSONObjectError) a}
  deriving (Functor, Applicative) via (Permutation (UP.Parser JSONObjectError))

newtype ReportingTupleParser a = ReportingTupleParser
  {runReportingTupleParser :: Integer -> (Integer, UP.Parser JSONArrayError a)}

instance Functor ReportingTupleParser where
  fmap f (ReportingTupleParser cb) =
    ReportingTupleParser $ \index -> second (f <$>) $ cb index

instance Applicative ReportingTupleParser where
  pure a = ReportingTupleParser (,pure a)
  (ReportingTupleParser f) <*> (ReportingTupleParser a) =
    ReportingTupleParser $ \index ->
      let (index', fp) = f index
          (index'', ap) = a index
       in ( index'',
            do
              f' <- fp
              when (index /= index' && index /= index'') comma
              f' <$> ap
          )

toObjectParser :: T.Text -> Parser JSONError a -> ReportingObjectParser a
toObjectParser field itemParser =
  ReportingObjectParser $
    asPermutationWithFailing parseKV failNoValue
  where
    failNoValue = do
      r <- UP.peekRest
      UP.failWith $
        MkJSONObjectError $ Map.singleton field ErrorNoValue
    parseKV = first (MkJSONObjectError . Map.singleton field) $ parseObjectKV field itemParser
{-# INLINE toObjectParser #-}

toObjectParserDef field itemParser def =
  ReportingObjectParser $
    asPermutationWithDefault parseKV def
  where
    parseKV = first (MkJSONObjectError . Map.singleton field) $ do
      parseObjectKV field itemParser

parseArrayInner :: UP.Parser JSONError a -> Integer -> UP.Parser JSONArrayError [a]
parseArrayInner parse index =
  ((:) <$> parseElem <*> ((comma *> parseArrayInner parse (index + 1)) <|> pure []))
    <|> pure []
  where
    parseElem = first (MkJSONArrayError . Map.singleton index) parse
{-# INLINE parseArrayInner #-}

parseDictKey :: UP.Parser JSONError a -> UP.Parser JSONObjectError (T.Text, a)
parseDictKey parseVal = do
  key <- textParser
  kvSep
  val <- first (MkJSONObjectError . Map.singleton key) parseVal
  pure (key, val)

instance JSONTupleParser ReportingTupleParser where
  consumeItemWith = \(ReportingParser itemParser) ->
    ReportingTupleParser $
      \index ->
        (index + 1, first (MkJSONArrayError . Map.singleton index) itemParser)

instance JSONObjectParser ReportingObjectParser where
  parseFieldWith field = \(ReportingParser itemParser) ->
    toObjectParser field itemParser
  parseFieldWithDefault field = \(ReportingParser itemParser) def ->
    toObjectParserDef field itemParser def

instance JSONParser ReportingParser where
  parseTuple (ReportingTupleParser tp) =
    ReportingParser $ do
      jt <- peekJSONType
      case jt of
        JSONTypeArray -> tuple
        other -> skipWithFailure $ ErrorBadType JSONTypeArray other
    where
      tuple = do
        startArray
        let (_, arrayParse) = tp 0
        arr <- first ErrorBadArray arrayParse
        endArray
        pure arr
  {-# INLINE parseTuple #-}
  parseTextConstant tc =
    ReportingParser $ do
      jt <- peekJSONType
      case jt of
        JSONTypeText -> textConstant
        other -> skipWithFailure $ ErrorBadType JSONTypeText other
    where
      textConstant = do
        r <- UP.specificWord 34
        void (parseSpecificKeyAfterQuote tc) <|> do
          r <- parseAfterQuote
          UP.failWith (ErrorBadTextConstant tc r)
  {-# INLINE parseTextConstant #-}
  parseArrayWith (ReportingParser rp) =
    ReportingParser $
      array
        <|> skipNullExpecting JSONTypeArray
        <|> skipBoolExpecting JSONTypeArray
        <|> skipTextExpecting JSONTypeArray
        <|> skipNumberExpecting JSONTypeArray
    where
      array = do
        startArray
        arr <- first ErrorBadArray $ parseArrayInner rp 0
        endArray
        pure arr
  {-# INLINE parseDictionary #-}
  parseDictionary (ReportingParser dict) =
    ReportingParser $ do
      jt <- peekJSONType
      case jt of
        JSONTypeObject -> parseDict
        other -> UP.asFailure $ skipAnything $> ErrorBadType JSONTypeObject other
    where
      parseDict = do
        startBracket
        r <- first ErrorBadObject $ parseDictKey dict `sepBy` comma
        endBracket
        pure r
  parseObject (ReportingObjectParser permute) = ReportingParser $ do
    r <- peekJSONType
    case r of
      JSONTypeObject -> po
      other -> UP.asFailure $ skipAnything $> ErrorBadType JSONTypeObject other
    where
      po = first ErrorBadObject $ do
        startBracket
        a <-
          wrapEffect
            skipAnyKV
            comma
            permute
        rest <- peekRest
        endBracket <|> do
          comma
          skipAnyKV `sepByVoid` comma
          endBracket
        pure a
  {-# INLINE parseObject #-}
  parseNull =
    ReportingParser $ do
      jt <- peekJSONType
      case jt of
        JSONTypeNull -> nullParser
        other -> skipWithFailure $ ErrorBadType JSONTypeNull other
  {-# INLINE parseNull #-}
  parseBool = ReportingParser $ do
    jt <- peekJSONType
    case jt of
      JSONTypeBool -> boolParser
      other -> skipWithFailure $ ErrorBadType JSONTypeBool other
  {-# INLINE parseBool #-}
  parseText = ReportingParser $ do
    jt <- peekJSONType
    case jt of
      JSONTypeText -> textParser
      other -> skipWithFailure $ ErrorBadType JSONTypeText other
  {-# INLINE parseText #-}
  parseNumber =
    ReportingParser $ do
      r <- peekJSONType
      case r of
        JSONTypeNumber -> scientific
        other -> UP.asFailure $ skipAnything $> ErrorBadType JSONTypeNumber other
  {-# INLINE parseNumber #-}
  validateJSON (ReportingParser rp) =
    ReportingParser $
      lowerErr (fmap (first ErrorMesage) rp)
  {-# INLINE validateJSON #-}

parseOrReportWith ::
  (forall parser. JSONParser parser => parser a) ->
  BS.ByteString ->
  Either JSONError a
parseOrReportWith (ReportingParser rp) bs =
  case UP.parseBS (UP.skipWhitespace *> rp) bs of
    Nothing -> Left ErrorInvalidJSON
    Just (AccumE r) -> r
{-# INLINE parseOrReportWith #-}

parseOrReport :: (FromJSON a) => BS.ByteString -> Either JSONError a
parseOrReport = parseOrReportWith fromJSON
{-# INLINE parseOrReport #-}
