{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Jordan.FromJSON.Megaparsec
    where

import Control.Applicative
import Control.Applicative.Combinators (sepBy)
import qualified Data.ByteString as ByteString
import Data.Char (chr, digitToInt, isControl, isHexDigit, ord)
import Data.Foldable (asum, traverse_)
import Data.Functor (void, ($>))
import Data.List (intercalate)
import Data.Monoid (Alt(..))
import Data.Scientific (Scientific(..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Void (Void)
import Data.Word (Word8)
import Debug.Trace (trace, traceM)
import Jordan.FromJSON.Class
import Jordan.FromJSON.ParseInternal
import Numeric (showHex)
import Text.Megaparsec ((<?>))
import qualified Text.Megaparsec as T
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = T.Parsec ErrorContext Text.Text
type ParseError = T.ParseErrorBundle Text.Text ErrorContext

newtype ErrorContext
  = ErrorContext { getErrorContext :: [Text.Text] }
  deriving (Show, Eq, Ord)

instance T.ShowErrorComponent ErrorContext where
  showErrorComponent
    = intercalate ", "
    . fmap (("in " ++) . Text.unpack)
    . getErrorContext

newtype ObjectParser a = ObjectParser { getObjectParser :: Permutation Parser a }
  deriving newtype (Functor, Applicative)

newtype ArrayParser a
  = ArrayParser { getArrayParser :: Parser a }
  deriving (Functor)

instance Applicative ArrayParser where
  pure = ArrayParser . pure
  (ArrayParser f) <*> (ArrayParser a) = ArrayParser $
    (f <* comma) <*> a

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme $ Lexer.space Char.space1 empty empty

takeSpace :: Parser ()
takeSpace = void $ many Char.space1

parseAnyField :: Parser ()
parseAnyField = T.label "an extraneous object field we do not care about" $ do
  T.label "ignored object key" parseJSONText
  lexeme $ Char.char ':'
  lexeme consumeJunkValue

objectKey :: Text.Text -> Parser ()
objectKey k = T.label ("object key '" <> Text.unpack k <> "'") $ lexeme $ do
  Char.char '"'
  T.label "object label" $
    Text.foldr (\c a -> parseCharInText c *> a) (pure ()) k
  Char.char '"'
  pure ()

parseCharInText :: Char -> Parser ()
parseCharInText a = parseLit a <|> escaped a
  where
    parseLit :: Char -> Parser ()
    parseLit = \case
      '\\' -> void $ T.chunk "\\\\"
      '"' -> void $ T.chunk "\\\""
      '/' -> void $ T.chunk "/" <|> T.chunk "\\/"
      '\b' -> void $ T.chunk "\\b"
      '\f' -> void $ T.chunk "\\f"
      '\n' -> void $ T.chunk "\\n"
      '\r' -> void $ T.chunk "\\r"
      '\t' -> void $ T.chunk "\\t"
      a -> if isControl a then empty else void $ T.single a
    escaped :: Char -> Parser ()
    escaped a = void $ T.chunk $ Text.pack $ withEscaped $ (showHex $ ord a) []
    withEscaped :: String -> String
    withEscaped a@[_] = "\\u000" <> a
    withEscaped a@[_,_] = "\\u00" <> a
    withEscaped a@[_,_,_] = "\\u0" <> a
    withEscaped r = "\\u" <> r

parseDictField
  :: Parser a
  -> Parser (Text.Text, a)
parseDictField valParser = do
  key <- parseJSONText
  labelSep
  val <- valParser
  pure (key, val)

parseObjectField
  :: Text.Text
  -> Parser a
  -> Parser a
parseObjectField t f = do
  T.try $ objectKey t
  labelSep
  lexeme f

parseJSONText :: Parser Text.Text
parseJSONText = lexeme $ do
  T.try $ Char.char '"'
  innerText

innerText :: Parser Text.Text
innerText = do
  chunk <- T.takeWhileP Nothing $ \char -> char /= '\\' && char /= '"'
  l <- T.lookAhead $ T.option Nothing (Just <$> T.anySingle)
  case l of
    Nothing -> fail "string without end"
    Just '"' -> do
      T.label "quotation mark" T.anySingle
      pure chunk
    Just '\\' -> do
      T.anySingle
      r <- parseEscape
      rest <- innerText
      pure $ chunk <> r <> rest
    Just _ -> fail "IMPOSSIBLE"

parseEscape :: Parser Text.Text
parseEscape
  = quote
  <|> backslash
  <|> solidus
  <|> backspace
  <|> formfeed
  <|> linefeed
  <|> carriage
  <|> tab
  <|> escapedUnicode
  where
    backslash = T.chunk "\\"
    quote = T.chunk "\""
    solidus = T.chunk "/" $> "/"
    backspace = T.chunk "b" $> "\b"
    formfeed = T.chunk "f" $> "\f"
    linefeed = T.chunk "n" $> "\n"
    carriage = T.chunk "r" $> "\r"
    tab = T.chunk "t" $> "\t"
    escapedUnicode = T.label "unicode escape code" $ do
      Char.char 'u'
      a <- parseHexDigit
      b <- parseHexDigit
      c <- parseHexDigit
      d <- parseHexDigit
      let s = (((a * 16) + b) * 16 + c) * 16 + d
      pure $ Text.pack [chr s]

parseHexDigit :: Parser Int
parseHexDigit = digitToInt <$> T.satisfy isHexDigit

comma :: Parser ()
comma = void $ lexeme $ Char.char ','

labelSep :: Parser ()
labelSep = void $ lexeme $ Char.char ':'

parseAnyObject :: Parser ()
parseAnyObject = T.label "Ignored object" $ do
  T.try $ lexeme $ Char.char '{'
  parseAnyField `sepBy` comma
  lexeme $ Char.char '}'
  pure ()

parseAnyArray :: Parser ()
parseAnyArray = T.label "Ignored array" $ do
  T.try $ lexeme $ Char.char '['
  consumeJunkValue `sepBy` comma
  lexeme $ Char.char ']'
  pure ()

consumeJunkValue :: Parser ()
consumeJunkValue
  = void parseAnyObject
  <|> void parseAnyArray
  <|> void parseJSONText
  <|> void parseJSONNumber
  <|> void parseJSONNull

parseJSONNumber :: Parser Scientific
parseJSONNumber = Lexer.signed (pure ()) Lexer.scientific

parseJSONNull :: Parser ()
parseJSONNull = void $ lexeme $ T.chunk "null"

parseJSONBool :: Parser Bool
parseJSONBool = lexeme $ (T.chunk "true" $> True) <|> (T.chunk "false" $> False)

junkFieldsAtEnd :: Parser ()
junkFieldsAtEnd = T.label "misc fields after parsing is done" $ do
  comma
  parseAnyField `sepBy` comma
  pure ()

newtype MegaparsecParser a
  = MegaparsecParser { getMegaparsecParser :: Parser a }
  deriving (Functor)
  deriving (Monoid) via (Alt Parser a)

instance Semigroup (MegaparsecParser a) where
  (MegaparsecParser a) <> (MegaparsecParser b) = MegaparsecParser $ T.try a <|> T.try b

instance JSONObjectParser ObjectParser where
  parseFieldWith label
    = ObjectParser
    . asPermutation
    . T.label ("field " <> Text.unpack label)
    . parseObjectField label
    . getMegaparsecParser

instance JSONTupleParser ArrayParser where
  consumeItemWith = ArrayParser . getMegaparsecParser

instance JSONParser MegaparsecParser where
  parseObject name p = MegaparsecParser $ T.label (Text.unpack name <> " object") $ do
    T.label "object start" $ lexeme $ Char.char '{'
    r <- wrapEffect parseAnyField comma $ getObjectParser p
    T.label "object end" $ T.optional junkFieldsAtEnd
    lexeme $ Char.char '}'
    pure r
  parseDictionary valParser = MegaparsecParser $ T.label "dictionary" $ do
    lexeme $ Char.char '{'
    r <- parseDictField (getMegaparsecParser valParser) `sepBy` comma
    lexeme $ Char.char '}'
    pure r
  parseTuple p = MegaparsecParser $ do
    lexeme $ T.label "Array start" $ Char.char '['
    r <- getArrayParser p
    lexeme $ T.label "Array end" $ Char.char ']'
    pure r
  parseArrayWith p = MegaparsecParser $ do
    lexeme $ T.label "Array start" $ Char.char '['
    r <- getMegaparsecParser p `sepBy` comma
    lexeme $ T.label "Array end" $ Char.char ']'
    pure r
  parseTextConstant t = MegaparsecParser $ T.label "text constant" $ do
    Char.char '"'
    Text.foldr (\c a -> parseCharInText c *> a) (pure ()) t
    Char.char '"'
    pure ()
  parseText = MegaparsecParser parseJSONText
  parseBool = MegaparsecParser parseJSONBool
  parseNumber = MegaparsecParser parseJSONNumber
  parseNull = MegaparsecParser $ T.label "null literal" parseJSONNull
  validateJSON (MegaparsecParser f) = MegaparsecParser $ do
    r <- f
    case r of
      Left a -> fail (Text.unpack a)
      Right a -> pure a

-- | Convert an abstract JSONParser to a Megaparsec parser.
convertParserToMegaparsecParser :: (forall parser. JSONParser parser => parser a) -> Parser a
convertParserToMegaparsecParser = getMegaparsecParser

-- | Get a megaparsec parser for your JSON value.
-- This parser will not construct any intermediate maps or other structures - your object will be parsed directly!
--
-- Note: this parser, until the ones that are built into the class, can consume whitespace at the start of the JSON.
megaparsecParser :: (FromJSON val) => Parser val
megaparsecParser = takeSpace *> getMegaparsecParser fromJSON

-- | Run an abstract JSONParser via Megaparsec.
runParserViaMegaparsec :: (forall parser. JSONParser parser => parser a) -> Text.Text -> Either String a
runParserViaMegaparsec p t =
  case T.runParser (convertParserToMegaparsecParser p) "" t of
    Left r -> Left $ T.errorBundlePretty r
    Right k -> Right k

-- | Parse an object for which 'FromJSON' is defined via Megaparsec.
parseViaMegaparsec :: forall val. (FromJSON val) => Text.Text -> Either String val
parseViaMegaparsec = runParserViaMegaparsec fromJSON
