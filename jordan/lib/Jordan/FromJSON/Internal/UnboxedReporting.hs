{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Jordan.FromJSON.Internal.UnboxedReporting where

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
import Debug.Trace (traceM)
import Jordan.FromJSON.Class
import Jordan.FromJSON.Internal.Attoparsec (bsToInteger)
import Jordan.FromJSON.Internal.Permutation
import Jordan.FromJSON.Internal.UnboxedParser as UP hiding (AccumE (..), AccumEL, AccumER)
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

skipWithFailure :: JSONError -> Parser JSONError a
skipWithFailure err =
  UP.asFailure $
    skipAnything $> err

lexeme :: Semigroup err => Parser err a -> Parser err a
lexeme p = p <* UP.skipWhitespace
{-# INLINE lexeme #-}

jsonTypeFromWord :: Word8 -> Maybe JSONType
jsonTypeFromWord jt
  | jt == 34 = pure JSONTypeText
  | jt == 116 || jt == 102 = pure JSONTypeBool
  | jt == 110 = pure JSONTypeNull
  | jt >= 48 && jt <= 57 = pure JSONTypeNumber
  | jt == 45 = pure JSONTypeNumber
  | jt == 91 = pure JSONTypeArray
  | jt == 123 = pure JSONTypeObject
  | otherwise = Nothing
{-# INLINE jsonTypeFromWord #-}

peekJSONType :: (Monoid err) => Parser err JSONType
peekJSONType = UP.orFail (jsonTypeFromWord <$> UP.peekWord)
{-# INLINE peekJSONType #-}

skipNullExpecting :: JSONType -> Parser JSONError a
skipNullExpecting jt =
  UP.asFailure $ nullParser $> ErrorBadType jt JSONTypeNull

-- | Parse a NULL value.
nullParser :: Semigroup err => Parser err ()
nullParser = lexeme $ UP.parseChunk "null" $> ()
{-# INLINE nullParser #-}

skipBoolExpecting :: JSONType -> Parser JSONError a
skipBoolExpecting jt =
  UP.asFailure $
    boolParser $> ErrorBadType jt JSONTypeBool
{-# INLINE skipBoolExpecting #-}

boolParser :: (Monoid err) => Parser err Bool
boolParser =
  lexeme $
    (UP.parseChunk "true" $> True)
      <|> (UP.parseChunk "false" $> False)
{-# INLINE boolParser #-}

skipTextExpecting :: JSONType -> Parser JSONError a
skipTextExpecting jt =
  UP.asFailure $
    textParser $> ErrorBadType jt JSONTypeText
{-# INLINE skipTextExpecting #-}

textParser :: (Monoid err) => Parser err T.Text
textParser = lexeme $ do
  UP.specificWord 34
  parseAfterQuote
{-# INLINE textParser #-}

sepByVoid :: Alternative f => f a1 -> f a2 -> f ()
sepByVoid elem sep = void $ sepBy (void elem) sep
{-# INLINE sepByVoid #-}

skipNumber :: (Monoid err) => Parser err ()
skipNumber = void scientific
{-# INLINE skipNumber #-}

skipNumberExpecting :: JSONType -> Parser JSONError a
skipNumberExpecting jt =
  UP.asFailure $
    skipNumber $> ErrorBadType jt JSONTypeNumber

skipAnything :: Monoid err => Parser err ()
skipAnything = do
  r <- UP.peekWord
  if
      | r == 110 -> lexeme $ UP.parseChunk "null"
      | r == 116 -> lexeme $ UP.parseChunk "true" -- t -> true
      | r == 102 -> lexeme $ UP.parseChunk "false"
      | r == 34 -> void textParser -- " -> text
      | r == 123 -> skipObject -- { -> object
      | r == 91 -> skipArray -- [ -> array
      | r == 45 || (r >= 48 && r <= 57) -> skipNumber
      | otherwise -> (orFail $ pure Nothing)
{-# INLINE skipAnything #-}

skipArray :: (Monoid err) => Parser err ()
skipArray = do
  startArray
  sepByVoid skipAnything comma
  endArray
{-# INLINE skipArray #-}

kvSep :: Semigroup err => Parser err ()
kvSep = lexeme $ UP.specificWord 58

skipAnyKV :: Monoid err => Parser err ()
skipAnyKV = do
  textParser
  kvSep
  skipAnything

comma :: Semigroup err => Parser err ()
comma = lexeme $ UP.specificWord 44

skipObject :: Monoid err => Parser err ()
skipObject = do
  lexeme $ UP.specificWord 123
  sepByVoid skipAnyKV comma
  lexeme $ UP.specificWord 125
{-# INLINE skipObject #-}

failOnError :: (Monoid err) => Either a T.Text -> Parser err T.Text
failOnError = \case
  Left _ -> failParse
  Right txt -> pure txt

parseAfterQuote :: (Monoid err) => Parser err T.Text
parseAfterQuote = do
  chunk <- UP.takeWord8Cont (\c -> c /= 92 && c /= 34) decodeUtf8'
  decoded <- failOnError chunk
  (lexeme (specificWord 34) $> decoded) <|> do
    specificWord 92
    escape <- parseEscape
    res <- parseAfterQuote
    pure $ decoded <> escape <> res
{-# INLINE parseAfterQuote #-}

hexDigit :: Semigroup err => Parser err Word8
hexDigit = do
  r <- UP.word
  orFail $
    if
        | r >= 48 && r <= 57 -> pure $ Just (r - 48)
        | r >= 97 && r <= 103 -> pure $ Just ((r - 97) + 10)
        | otherwise -> pure Nothing
{-# INLINE hexDigit #-}

parseEscape :: (Monoid err) => UP.Parser err T.Text
parseEscape =
  quote
    <|> backslash
    <|> solidus
    <|> backspace
    <|> formfeed
    <|> linefeed
    <|> carriage
    <|> tab
    <|> unicode
  where
    quote = specificWord 34 $> "\""
    backslash = specificWord 92 $> "\\"
    solidus = specificWord 47 $> "/"
    backspace = specificWord 98 $> "\b"
    formfeed = specificWord 102 $> "\f"
    linefeed = specificWord 110 $> "\n"
    carriage = specificWord 114 $> "\r"
    tab = specificWord 116 $> "\t"
    unicode = do
      specificWord 117
      a <- hexDigit
      b <- hexDigit
      c <- hexDigit
      d <- hexDigit
      let res = (((fromIntegral a * 16) + fromIntegral b) * 16 + fromIntegral c) * 16 + fromIntegral d
      pure $ T.pack [chr res]
{-# INLINE parseEscape #-}

parseCharInText (c :: Char) = parseLit c <|> escaped c
  where
    parseLit = \case
      '\\' -> UP.parseChunk "\\\\"
      '"' -> UP.parseChunk "\\\""
      '/' -> UP.parseChunk "/" <|> UP.parseChunk "\\/"
      '\b' -> UP.parseChunk "\\b"
      '\f' -> UP.parseChunk "\\f"
      '\n' -> UP.parseChunk "\\n"
      '\r' -> UP.parseChunk "\\r"
      '\t' -> UP.parseChunk "\\t"
      a -> if isControl a then empty else UP.parseChunk (encodeUtf8 $ T.singleton a)
    escaped c = UP.parseChunk $ encodeUtf8 $ "\\u" <> T.justifyRight 4 '0' (T.pack $ showHex (ord c) mempty)
{-# INLINE parseCharInText #-}

parseSpecificKeyInQuotes :: Monoid err => T.Text -> Parser err ()
parseSpecificKeyInQuotes t = UP.specificWord 34 *> parseSpecificKeyAfterQuote t
{-# INLINE parseSpecificKeyInQuotes #-}

parseSpecificKeyAfterQuote :: Monoid err => T.Text -> Parser err ()
parseSpecificKeyAfterQuote key = (parseRaw <|> parseChars) *> lexeme (UP.specificWord 34)
  where
    parseChars = T.foldr (\c a -> parseCharInText c *> a) (pure ()) key
    parseRaw =
      if isJust $ T.findIndex invalidTextChar key
        then empty
        else UP.parseChunk (encodeUtf8 key)

startBracket :: Semigroup err => Parser err ()
startBracket = lexeme $ UP.specificWord 123

endBracket :: Semigroup err => Parser err ()
endBracket = lexeme $ UP.specificWord 125

startArray :: Semigroup err => Parser err ()
startArray = lexeme $ UP.specificWord 91

endArray :: Semigroup err => Parser err ()
endArray = lexeme $ UP.specificWord 93

parseObjectKV :: Monoid err => T.Text -> Parser err b -> Parser err b
parseObjectKV key v = do
  lexeme $ parseSpecificKeyInQuotes key
  lexeme $ UP.specificWord 58
  v

invalidTextChar :: Char -> Bool
invalidTextChar c =
  c == '"'
    || c == '\\'
    || isControl c

data SP = SP !Integer {-# UNPACK #-} !Int

-- A sub-quadratic algorithm for Integer. Pairs of adjacent radix b
-- digits are combined into a single radix b^2 digit. This process is
-- repeated until we are left with a single digit. This algorithm
-- performs well only on large inputs, so we use the simple algorithm
-- for smaller inputs.
valInteger :: Integer -> Int -> [Integer] -> Integer
valInteger = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ [] = 0
    go _ _ [d] = d
    go b l ds
      | l > 40 = b' `seq` go b' l' (combine b ds')
      | otherwise = valSimple b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2

    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ [] = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"
{-# INLINE valInteger #-}

-- The following algorithm is only linear for types whose Num operations
-- are in constant time.
valSimple :: Integer -> [Integer] -> Integer
valSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
{-# INLINE valSimple #-}

isDigitWord8 :: Word8 -> Bool
isDigitWord8 c = c >= 48 && c <= 57
{-# INLINE isDigitWord8 #-}

decimal0 :: Semigroup err => Parser err Integer
decimal0 = do
  let zero = 48
  digits <- UP.takeWord8 isDigitWord8
  let !length = BS.length digits
  when (length == 0) UP.failParse
  if length > 1 && BS.unsafeHead digits == zero
    then UP.failParse
    else return (bsToInteger digits)
{-# INLINE decimal0 #-}

scientific :: (Monoid err) => UP.Parser err Scientific
scientific = lexeme $ do
  let minus = 45
      plus = 43
  sign <- UP.peekWord
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void UP.word
  n <- decimal0
  let f fracDigits =
        SP
          (BS.foldl' step n fracDigits)
          (negate $ BS.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)
  dotty <- UP.peekWordMaybe
  SP c e <- case dotty of
    Just 46 -> UP.word *> UP.takeWord81Cont isDigitWord8 f
    _ -> pure (SP n 0)
  let !signedCoeff
        | positive = c
        | otherwise = - c
  ( (UP.specificWord 101 <|> UP.specificWord 69)
      *> fmap (Scientific.scientific signedCoeff . (e +)) (UP.signed (snd <$> UP.parseIntegral))
    )
    <|> pure (Scientific.scientific signedCoeff e)
{-# INLINE scientific #-}
