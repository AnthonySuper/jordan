{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Implementation of FromJSON parsers via Attoparsec.
--
-- This module does not construct intermediate data structures like maps or key-value lists,
-- and instead uses permutation parsers in order to parse your data structure directly.
module Jordan.FromJSON.Attoparsec
    ( convertParserToAttoparsecParser
    , runParserViaAttoparsec
    , parseViaAttoparsec
    , attoparsecParser
    ) where

import Control.Applicative (Alternative(..))
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
import Data.Monoid (Alt(..))
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Jordan.FromJSON.Class
import Jordan.FromJSON.ParseInternal
import Numeric (showHex)
import qualified Text.Megaparsec as Text

newtype ObjectParser a
  = ObjectParser
  { runObjectParser :: Permutation AP.Parser a }
  deriving (Functor, Applicative)

newtype ArrayParser a
  = ArrayParser
  { runArrayParser :: AP.Parser a }
  deriving (Functor)

instance Applicative ArrayParser where
  pure = ArrayParser . pure
  f <*> a = ArrayParser $ do
    f' <- runArrayParser f
    comma
    a' <- runArrayParser a
    pure $ f' a'

skipSpace :: AP.Parser ()
skipSpace = AP.skipWhile isSpace
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
parseAnyField = label "ignored object field" $ void $ do
  lexeme parseJSONText
  labelSep
  lexeme anyDatum

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
junkInnerText = do
  AP.skipWhile $ \char -> char /= 92 && char /= 34
  l <- AP.peekWord8
  case l of
    Nothing -> fail "string without end"
    Just 34 -> do
      AP.anyWord8
      pure ()
    Just 93 -> do
      AP.anyWord8
      parseEscape
      junkInnerText
    Just _ -> fail "Impossible: Skipped until we parsed a '\\' or a '\"', yet next char was neither"

parseEscape :: AP.Parser Text.Text
parseEscape
  = quote
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
    withEscaped a@[_,_] = "\\u00" <> a
    withEscaped a@[_,_,_] = "\\u0" <> a
    withEscaped r = "\\u" <> r

objectKey :: Text.Text -> AP.Parser ()
objectKey k = lexeme $ do
  quotation
  Text.foldr (\c a -> parseCharInText c *> a) (pure ()) k
  quotation
  pure ()

startObject :: AP.Parser ()
startObject
  = label "object starting brace ('{')"
  $ lexeme
  $ void
  $ AP.word8 123

endObject :: AP.Parser ()
endObject
  = label "object ending brace ('}')"
  $ lexeme
  $ void
  $ AP.word8 125

inObjectBraces :: AP.Parser a -> AP.Parser a
inObjectBraces interior = startObject *> interior <* endObject

startArray :: AP.Parser ()
startArray
  = label "array starting brace ('[')"
  $ lexeme
  $ void
  $ AP.word8 91

endArray :: AP.Parser ()
endArray
  = label "array ending brace (']')"
  $ lexeme
  $ void
  $ AP.word8 93

labelSep :: AP.Parser ()
labelSep = label "key-value separator (':')" $ void $ lexeme $ AP.string ":"

anyDatum :: AP.Parser ()
anyDatum = lexeme inner
  where
    inner
      = runAttoparsecParser parseNull
      <|> void (runAttoparsecParser parseBool)
      <|> parseJunkText
      <|> void (runAttoparsecParser parseNumber)
      <|> void (runAttoparsecParser parseBool)
      <|> anyObject
      <|> anyArray

anyArray :: AP.Parser ()
anyArray = label "ignored array" $ void $ do
  startArray
  endArray <|> junkItems
    where
      junkItems = do
        anyDatum
        endArray <|> (comma *> junkItems)

number :: AP.Parser Scientific
number = CH.scientific

anyObject :: AP.Parser ()
anyObject = label "ignored object" $ void $ do
  startObject
  endObject <|> junkField
    where
      junkField = do
        parseAnyField
        endObject <|> (comma *> junkField)

parseObjectField
  :: Text.Text
  -> AP.Parser a
  -> AP.Parser a
parseObjectField t f = do
  objectKey t
  labelSep
  lexeme f

parseDictField
  :: AP.Parser a
  -> AP.Parser (Text.Text, a)
parseDictField p = do
  key <- parseJSONText
  labelSep
  val <- p
  pure (key, val)

instance JSONObjectParser ObjectParser where
  parseFieldWith label
    = ObjectParser
    . asPermutation
    . parseObjectField label
    . runAttoparsecParser
  {-# INLINE parseFieldWith #-}

newtype AttoparsecParser a
  = AttoparsecParser
  { runAttoparsecParser :: AP.Parser a }
  deriving (Functor)
  deriving (Semigroup, Monoid) via (Alt AP.Parser a)

instance JSONTupleParser ArrayParser where
  consumeItemWith = ArrayParser . runAttoparsecParser

instance JSONParser AttoparsecParser where
  parseObject l p = AttoparsecParser $ label ("Object '" <> Text.unpack l <> "'") $ do
    startObject
    r <- wrapEffect (parseAnyField <?> "ignored field in the middle of an object") comma $ runObjectParser p
    objectEndWithJunk
    pure r
  {-# INLINE parseObject #-}
  parseDictionary parse = AttoparsecParser $ inObjectBraces $ do
    parseDictField (runAttoparsecParser parse) `AP.sepBy` comma
  parseTextConstant c = AttoparsecParser (objectKey c <?> "text constant" <> Text.unpack c)
  {-# INLINE parseTextConstant #-}
  parseText = AttoparsecParser parseJSONText
  {-# INLINE parseText #-}
  parseNumber = AttoparsecParser number
  {-# INLINE parseNumber #-}
  validateJSON v = AttoparsecParser $ do
    r <- runAttoparsecParser v
    case r of
      Left err -> fail (Text.unpack err)
      Right e -> pure e
  parseTuple ap = AttoparsecParser $ do
    lexeme $ AP.word8 91
    r <- runArrayParser ap
    lexeme $ AP.word8 93
    pure r
  {-# INLINE parseTuple #-}
  parseArrayWith jp = AttoparsecParser $ do
    startArray
    r <- lexeme (runAttoparsecParser jp) `AP.sepBy` comma <?> "array items"
    endArray
    pure r
  {-# INLINE parseArrayWith #-}
  parseBool = AttoparsecParser $ lexeme $
    (AP.string "true" $> True) <|> (AP.string "false" $> False)
  parseNull = AttoparsecParser $ lexeme (AP.string "null" $> ())
  nameParser l = \(AttoparsecParser a) -> AttoparsecParser $
    label ("Parser '" <> Text.unpack l <> "'") a

-- | Convert an abstract JSON parser to an Attoparsec Parser.
-- This function will skip leading whitespace.
convertParserToAttoparsecParser :: (forall parser. JSONParser parser => parser a) -> AP.Parser a
convertParserToAttoparsecParser = (skipSpace *>) .  runAttoparsecParser

runParserViaAttoparsec :: (forall parser. JSONParser parser => parser a) -> ByteString -> Either String a
runParserViaAttoparsec p = AP.parseOnly (convertParserToAttoparsecParser p)

-- | Parse a ByteString via an Attoparsec Parser.
parseViaAttoparsec :: (FromJSON val) => ByteString -> Either String val
parseViaAttoparsec = AP.parseOnly (skipSpace *> runAttoparsecParser fromJSON)

-- | Get an Attoparsec parser for a particular JSON-parsable value.
attoparsecParser :: (FromJSON val) => AP.Parser val
attoparsecParser = runAttoparsecParser fromJSON
