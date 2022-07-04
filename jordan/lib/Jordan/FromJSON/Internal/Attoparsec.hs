{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Jordan.FromJSON.Internal.Attoparsec where

import Control.Applicative (Alternative (..))
import Control.Monad (void, when)
import Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Char (chr, digitToInt, isControl, isHexDigit, ord)
import Data.Functor (void, ($>))
import Data.Monoid (Alt (..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Numeric (showHex)

skipSpace :: AP.Parser ()
skipSpace = AP.skipWhile isSpace <?> "skipped space"
  where
    isSpace = \case
      32 -> True
      10 -> True
      13 -> True
      9 -> True
      _ -> False

lexeme :: AP.Parser a -> AP.Parser a
lexeme a = a <* skipSpace

label :: String -> AP.Parser a -> AP.Parser a
label l p = p <?> l

parseAnyField :: AP.Parser ()
parseAnyField =
  {-# SCC ignoredObjectField #-}
  label "ignored object field" $
    void $ do
      lexeme parseJunkText
      labelSep
      anyDatum

objectEndWithJunk :: AP.Parser ()
objectEndWithJunk = endObject <|> junkFieldAndEnd
  where
    junkFieldAndEnd = void $ do
      comma
      label "ignored extra field at object end" parseAnyField
      objectEndWithJunk

comma :: AP.Parser ()
comma = label "comma character" $ void $ lexeme (AP.string ",")

quotation :: AP.Parser ()
quotation = label "quotation mark" $ void $ AP.word8 34

parseJSONText :: AP.Parser Text.Text
parseJSONText = label "JSON text" $ do
  quotation
  innerText

-- | A parser for a JSON text value that skips its input.
-- Avoids doing UTF-8 Decoding.
parseJunkText :: AP.Parser ()
parseJunkText = label "Ignored JSON Text Literal" $ do
  quotation
  junkInnerText
{-# INLINE parseJunkText #-}

-- | Parses the bit of a JSON string after the quotation.
innerText :: AP.Parser Text.Text
innerText = do
  chunk <- label "Skipped text body" $
    AP.takeWhile $ \char -> char /= 92 && char /= 34
  l <- AP.peekWord8
  case l of
    Nothing -> fail "string without end"
    Just 34 -> do
      AP.anyWord8
      pure $ decodeUtf8 chunk
    Just 92 -> do
      AP.anyWord8
      r <- label "escape value" parseEscape
      rest <- innerText
      pure $ decodeUtf8 chunk <> r <> rest
    Just _ -> fail "Impossibe: Parsed until we parsed a '\\' or a '\"', yet next char was neither"

junkInnerText :: AP.Parser ()
junkInnerText =
  {-# SCC ignoredTextBetweenQuotes #-}
  do
    AP.skipWhile $ \char -> char /= 92 && char /= 34
    !l <- AP.peekWord8
    case l of
      Nothing -> fail "string without end"
      Just 34 -> AP.anyWord8 $> ()
      Just 93 -> do
        AP.anyWord8
        parseEscape
        -- Yes we could save a miniscule amount of time by replacing this with an "ignoring" version.
        -- However, laziness means that we probably don't save *that* much.
        junkInnerText
      Just _ -> fail "Impossible: Skipped until we parsed a '\\' or a '\"', yet next char was neither"
{-# INLINE junkInnerText #-}

parseEscape :: AP.Parser Text.Text
parseEscape =
  quote
    <|> backslash
    <|> solidus
    <|> backspace
    <|> formfeed
    <|> linefeed
    <|> carriage
    <|> tab
    <|> escaped
  where
    backslash = AP.string "\\" $> "\\" <?> "Backslash escape"
    quote = AP.string "\"" $> "\"" <?> "Quote escape"
    solidus = AP.string "/" $> "/" <?> "Solidus escape"
    backspace = AP.string "b" $> "\b" <?> "Backspace escape"
    formfeed = AP.string "f" $> "\f" <?> "Formfeed escape"
    linefeed = AP.string "n" $> "\n" <?> "Linefeed escape"
    carriage = AP.string "r" $> "\r" <?> "Carriage escape"
    tab = AP.string "t" $> "\t" <?> "Tab escape"
    escaped = label "UTF Code Escape" $ do
      AP.string "u"
      a <- parseHexDigit
      b <- parseHexDigit
      c <- parseHexDigit
      d <- parseHexDigit
      let s = (((a * 16) + b) * 16 + c) * 16 + d
      pure $ Text.pack [chr s]

parseHexDigit :: AP.Parser Int
parseHexDigit = label "hex digit" (digitToInt <$> CH.satisfy isHexDigit)

parseCharInText :: Char -> AP.Parser ()
parseCharInText a = parseLit a <|> escaped a
  where
    parseLit :: Char -> AP.Parser ()
    parseLit = \case
      '\\' -> void $ AP.string "\\\\"
      '"' -> void $ AP.string "\\\""
      '/' -> void $ AP.string "/" <|> AP.string "\\/"
      '\b' -> void $ AP.string "\\b"
      '\f' -> void $ AP.string "\\f"
      '\n' -> void $ AP.string "\\n"
      '\r' -> void $ AP.string "\\r"
      '\t' -> void $ AP.string "\\t"
      a -> if isControl a then empty else void $ AP.string $ encodeUtf8 $ Text.singleton a
    escaped :: Char -> AP.Parser ()
    escaped a = void $ AP.string $ encodeUtf8 $ Text.pack $ withEscaped $ (showHex $ ord a) []
    withEscaped :: String -> String
    withEscaped a@[_] = "\\u000" <> a
    withEscaped a@[_, _] = "\\u00" <> a
    withEscaped a@[_, _, _] = "\\u0" <> a
    withEscaped r = "\\u" <> r

mustBeEscaped :: Char -> Bool
mustBeEscaped = \case
  '\\' -> True
  '"' -> True
  '/' -> True
  '\b' -> True
  '\f' -> True
  '\n' -> True
  '\r' -> True
  '\t' -> True
  _ -> False

canParseDirectly :: Text.Text -> Bool
canParseDirectly t = not $ Text.foldr (\c v -> v || mustBeEscaped c) False t

parseTextBody :: Text.Text -> AP.Parser ()
parseTextBody text
  | canParseDirectly text = void (A.string (encodeUtf8 text)) <|> parseViaChars text
  | otherwise = parseViaChars text

parseViaChars = Text.foldr (\c a -> parseCharInText c *> a) (pure ())

objectKey :: Text.Text -> AP.Parser ()
objectKey k = lexeme $ do
  quotation
  {-# SCC "knownObjectKeyBetweenQuotes" #-} parseTextBody k
  quotation
  pure ()

startObject :: AP.Parser ()
startObject =
  label "object starting brace ('{')" $
    lexeme $
      void $
        AP.word8 123

endObject :: AP.Parser ()
endObject =
  label "object ending brace ('}')" $
    lexeme $
      void $
        AP.word8 125

inObjectBraces :: AP.Parser a -> AP.Parser a
inObjectBraces interior = startObject *> interior <* endObject

startArray :: AP.Parser ()
startArray =
  label "array starting brace ('[')" $
    lexeme $
      void $
        AP.word8 91

endArray :: AP.Parser ()
endArray =
  label "array ending brace (']')" $
    lexeme $
      void $
        AP.word8 93

labelSep :: AP.Parser ()
labelSep = label "key-value separator (':')" $ void $ lexeme $ AP.string ":"

anyDatum :: AP.Parser ()
anyDatum =
  lexeme $
    {-# SCC "ignoredJSONValue" #-}
    do
      t <- AP.peekWord8
      case t of
        Just 102 -> void $ AP.string "false"
        Just 110 -> void $ AP.string "null"
        Just 116 -> void $ AP.string "true"
        Just 123 -> anyObject
        Just 34 -> parseJunkText
        Just 43 -> parseJunkNumber
        Just 45 -> parseJunkNumber
        Just 48 -> parseJunkNumber
        Just 49 -> parseJunkNumber
        Just 50 -> parseJunkNumber
        Just 51 -> parseJunkNumber
        Just 52 -> parseJunkNumber
        Just 53 -> parseJunkNumber
        Just 54 -> parseJunkNumber
        Just 55 -> parseJunkNumber
        Just 57 -> parseJunkNumber
        Just 59 -> parseJunkNumber
        Just 91 -> anyArray
        Just _ -> fail "not a valid starter of any JSON value"
        Nothing -> fail "empty input"
{-# INLINE anyDatum #-}

anyArray :: AP.Parser ()
anyArray = label "ignored array" $
  void $ do
    startArray
    endArray <|> junkItems
  where
    junkItems = do
      anyDatum
      endArray <|> (comma *> junkItems)

parseJunkDecimalZero :: AP.Parser ()
parseJunkDecimalZero = do
  let zero = 48
  digits <- A.takeWhile1 CH.isDigit_w8
  when (B.length digits > 1 && B.unsafeHead digits == zero) $
    fail "leading zero"

parseJunkExponent :: AP.Parser ()
parseJunkExponent = label "junk exponent" $ do
  A.satisfy (\ex -> ex == 101 || ex == 69)
  A.skipWhile (\ch -> ch == 45 || ch == 43)
  parseJunkDecimalZero

parseJunkNumber :: AP.Parser ()
parseJunkNumber = do
  A.skipWhile (\ch -> ch == 45 || ch == 43)
  parseJunkDecimalZero
  -- skip decimal
  dot <- A.peekWord8
  case dot of
    Just 46 -> void $ A.anyWord8 *> A.takeWhile1 CH.isDigit_w8
    _ -> pure ()
  parseJunkExponent <|> pure ()

------ Scientific parser, copy/pasted from Aeson. ----

-- (This parser was in turn copy-pasted itself from various soruces so this is kohser)

-- A strict pair
data SP = SP !Integer {-# UNPACK #-} !Int

decimal0 :: AP.Parser Integer
decimal0 = do
  let zero = 48
  digits <- A.takeWhile1 CH.isDigit_w8
  if B.length digits > 1 && B.unsafeHead digits == zero
    then fail "leading zero"
    else return (bsToInteger digits)

-- | Parse a JSON number.
--
-- This function is wholesale copy/pasted from Aeson.
-- Thanks to them.
scientific :: AP.Parser Scientific
scientific = do
  let minus = 45
      plus = 43
  sign <- A.peekWord8'
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void A.anyWord8

  n <- decimal0

  let f fracDigits =
        SP
          (B.foldl' step n fracDigits)
          (negate $ B.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)

  dotty <- A.peekWord8
  -- '.' -> ascii 46
  SP c e <- case dotty of
    Just 46 -> A.anyWord8 *> (f <$> A.takeWhile1 CH.isDigit_w8)
    _ -> pure (SP n 0)

  let !signedCoeff
        | positive = c
        | otherwise = - c

  let littleE = 101
      bigE = 69
  ( A.satisfy (\ex -> ex == littleE || ex == bigE)
      *> fmap (Scientific.scientific signedCoeff . (e +)) (CH.signed CH.decimal)
    )
    <|> return (Scientific.scientific signedCoeff e)
{-# INLINE scientific #-}

bsToInteger :: B.ByteString -> Integer
bsToInteger bs
  | l > 40 = valInteger 10 l [fromIntegral (w - 48) | w <- B.unpack bs]
  | otherwise = bsToIntegerSimple bs
  where
    l = B.length bs

bsToIntegerSimple :: B.ByteString -> Integer
bsToIntegerSimple = B.foldl' step 0
  where
    step a b = a * 10 + fromIntegral (b - 48) -- 48 = '0'

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

-- The following algorithm is only linear for types whose Num operations
-- are in constant time.
valSimple :: Integer -> [Integer] -> Integer
valSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d

number :: AP.Parser Scientific
number = scientific
{-# INLINE number #-}

anyObject :: AP.Parser ()
anyObject =
  {-# SCC ignoredJSONObject #-}
  label "ignored object" $
    void $ do
      startObject
      endObject <|> junkField
  where
    junkField = do
      parseAnyField
      endObject <|> (comma *> junkField)

parseObjectField ::
  Text.Text ->
  AP.Parser a ->
  AP.Parser a
parseObjectField t f = do
  objectKey t
  labelSep
  lexeme f

parseDictField ::
  AP.Parser a ->
  AP.Parser (Text.Text, a)
parseDictField p = do
  key <- lexeme parseJSONText
  labelSep
  val <- p
  pure (key, val)
