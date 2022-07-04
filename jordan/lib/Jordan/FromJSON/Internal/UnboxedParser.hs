{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | A parser module using unboxed types for speed.
--
-- This is done because parsing to a JSON error report needs custom handling.
module Jordan.FromJSON.Internal.UnboxedParser where

import Control.Applicative
import Control.Monad (when)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Functor
import Data.Monoid (Alt (..))
import Data.Word
import Debug.Trace (trace)
import GHC.Exts
import GHC.ForeignPtr
import GHC.Prim
import GHC.Types
import GHC.Word
import qualified Jordan.Types.Internal.AccumE as AE
import Jordan.Types.JSONError (JSONArrayError, JSONError, JSONObjectError)
import System.IO.Unsafe

#if __GLASGOW_HASKELL__ > 900
-- | Type of Word8 in GHC prim land. On GHC > 9.0, this is its own type.
type WordPrim = Word8#
#elif __GLASGOW_HASKELL__ > 800
-- | Type of word8 in GHC prim land.
-- On the GHC 8 series, this is 'Word#'.
type WordPrim = Word#
#endif

-- | Newtype wrapper around the state of an input.
--
-- This is just the offset into the buffer.
newtype InputState = InputState# {getInputOffset :: Int#}

-- | Pattern synonym so we can use our above unlifted newtype like a record, if we do desire.
pattern InputState :: Int# -> InputState
pattern InputState {offset} =
  InputState# offset

{-# COMPLETE InputState #-}

maxOffset :: InputState -> InputState -> InputState
maxOffset (InputState# lhs) (InputState# rhs) =
  InputState#
    (if isTrue# (lhs ># rhs) then lhs else rhs)

-- | Environment of a parser.
--
-- This is basically unpacked parts of a ByteString.
newtype InputRead = InputRead# {getInputRead :: (# ForeignPtrContents, Addr#, Int# #)}

pattern InputRead :: ForeignPtrContents -> Addr# -> Int# -> InputRead
pattern InputRead {foreignPtr, addr, endOffset} = InputRead# (# foreignPtr, addr, endOffset #)

{-# COMPLETE InputRead #-}

-- | Unboxed type similar to 'Jordan.Types.Internal.AccumE'.
newtype AccumE err a = AccumE {getAccumE :: (# err| a #)}

pattern AccumER :: a -> AccumE err a
pattern AccumER a = AccumE (# | a #)

pattern AccumEL :: err -> AccumE err a
pattern AccumEL err = AccumE (# err | #)

{-# COMPLETE AccumEL, AccumER #-}

newtype ParseResult# err res = ParseResult# {getParseResult# :: (# (# InputState, AccumE err res #)| (# #) #)}

pattern JustParseResult :: InputState -> AccumE err res -> ParseResult# err res
pattern JustParseResult {inputState, res} = ParseResult# (# (# inputState, res #) | #)

pattern NoParseResult :: ParseResult# err res
pattern NoParseResult = ParseResult# (# | (##) #)

bimapAcc :: (err -> err') -> (a -> a') -> AccumE err a -> AccumE err' a'
bimapAcc first _ (AccumEL a) = AccumEL (first a)
bimapAcc _ second (AccumER a) = AccumER (second a)

eitherAcc :: Either err a -> AccumE err a
eitherAcc (Left e) = AccumEL e
eitherAcc (Right a) = AccumER a

appAcc ::
  Semigroup err =>
  AccumE err (a1 -> a2) ->
  AccumE err a1 ->
  AccumE err a2
appAcc (AccumER f) (AccumER a) = AccumER (f a)
appAcc (AccumEL lhs) (AccumEL rhs) = AccumEL (lhs <> rhs)
appAcc (AccumEL lhs) _ = AccumEL lhs
appAcc _ (AccumEL rhs) = AccumEL rhs

accSet :: a1 -> AccumE err a2 -> AccumE err a1
accSet a (AccumER _) = AccumER a
accSet _ (AccumEL err) = AccumEL err

{-# COMPLETE JustParseResult, NoParseResult #-}

-- | We need a parser with *error recovery*.
-- So the basic idea is that we separate errors reported during parsing from errors that make parsing stop.
-- IE, if we expect a JSON null but we get a JSON string, and the string is well-formed, we can keep parsing, but we
-- will *report* an error.
newtype Parser# s err res = Parser# {runParser :: InputRead -> InputState -> State# s -> (# State# s, ParseResult# err res #)}

bimapParser :: (err -> err') -> (a -> a') -> Parser# s err a -> Parser# s err' a'
bimapParser l r (Parser# cb) = Parser# $ \env input s ->
  case cb env input s of
    (# s', a #) ->
      (#
        s',
        case a of
          NoParseResult -> NoParseResult
          JustParseResult is e -> JustParseResult is (bimapAcc l r e)
      #)

fmapParser :: (a -> res) -> Parser# s err a -> Parser# s err res
fmapParser f (Parser# cb) = Parser# $ \env input s ->
  case cb env input s of
    (# s', a #) ->
      (#
        s,
        case a of
          NoParseResult -> NoParseResult
          JustParseResult is (AccumER !r) -> JustParseResult is (AccumER (f r))
          JustParseResult is (AccumEL !l) -> JustParseResult is (AccumEL l)
      #)
{-# INLINE fmapParser #-}

pureParser :: (Semigroup err) => a -> Parser# s err a
pureParser !a = Parser# $ \_ state s -> (# s, JustParseResult state (AccumER a) #)
{-# INLINE pureParser #-}

apParser :: (Semigroup err) => Parser# s err (a -> b) -> Parser# s err a -> Parser# s err b
apParser (Parser# fcb) (Parser# acb) = Parser# $ \env input s ->
  case fcb env input s of
    (# s', NoParseResult #) -> (# s', NoParseResult #)
    (# s', JustParseResult !input' !f #) ->
      case acb env input' s' of
        (# s'', a #) ->
          (#
            s'',
            case a of
              NoParseResult -> NoParseResult
              JustParseResult !input'' !a -> JustParseResult input'' (f `appAcc` a)
          #)
{-# SPECIALIZE apParser :: Parser# s JSONError (a -> b) -> Parser# s JSONError a -> Parser# s JSONError b #-}
{-# SPECIALIZE apParser :: Parser# s JSONObjectError (a -> b) -> Parser# s JSONObjectError a -> Parser# s JSONObjectError b #-}
{-# SPECIALIZE apParser :: Parser# s JSONArrayError (a -> b) -> Parser# s JSONArrayError a -> Parser# s JSONArrayError b #-}
{-# INLINE apParser #-}

-- | Alternative instance for a parser.
-- This has weird behavior in that, if we have two results with delayed errors, this will act as if it skipped the *largest* amount
-- of said errors.
altParser :: (Monoid err) => Parser# s err a -> Parser# s err a -> Parser# s err a
altParser (Parser# lhs) (Parser# rhs) = Parser# $ \env input s ->
  let (# s', !lhs' #) = lhs env input s
   in case lhs' of
        -- If the parser failed to parse, we also fail to parse
        NoParseResult -> rhs env input s'
        JustParseResult state (AccumER !res) -> (# s', JustParseResult state (AccumER res) #)
        JustParseResult state res@(AccumEL !err) ->
          case rhs env input s' of
            (# s'', NoParseResult #) -> (# s'', JustParseResult state res #)
            (# s'', JustParseResult state' (AccumER !res) #) -> (# s'', JustParseResult (maxOffset state state') (AccumER res) #)
            (# s'', JustParseResult state' (AccumEL !err') #) ->
              (# s'', JustParseResult (maxOffset state state') (AccumEL (err <> err')) #)
{-# SPECIALIZE altParser :: Parser# s JSONError a -> Parser# s JSONError a -> Parser# s JSONError a #-}
{-# SPECIALIZE altParser :: Parser# s JSONObjectError a -> Parser# s JSONObjectError a -> Parser# s JSONObjectError a #-}
{-# SPECIALIZE altParser :: Parser# s JSONArrayError a -> Parser# s JSONArrayError a -> Parser# s JSONArrayError a #-}
{-# INLINE altParser #-}

-- | Monadic bind for these parsers.
--
-- Note that this breaks the monad laws, as we do more error accumulation with (<*>) than we do ap.
-- Oh well.
bindParser :: Parser# s err a -> (a -> Parser# s err b) -> Parser# s err b
bindParser (Parser# arg) cont =
  Parser# $
    {-# SCC unboxedParserBindInner #-}
    \env state s ->
      let (# s', result #) = arg env state s
       in case result of
            NoParseResult -> (# s', NoParseResult #)
            JustParseResult state' (AccumEL err) -> (# s', JustParseResult state (AccumEL err) #)
            JustParseResult state' (AccumER r) -> runParser (cont r) env state' s'
{-# INLINE bindParser #-}

emptyParser :: Parser# s err a
emptyParser = Parser# $ \_ _ s -> (# s, NoParseResult #)

newtype Parser err res = Parser {getParser :: Parser# RealWorld err res}
  deriving (Semigroup, Monoid) via (Alt (Parser err) res)

instance Bifunctor Parser where
  bimap l r (Parser p) = Parser (bimapParser l r p)
  {-# INLINE bimap #-}

instance Functor (Parser err) where
  fmap f (Parser a) = Parser (fmapParser f a)
  {-# INLINE fmap #-}
  a <$ (Parser (Parser# cb)) = Parser $
    Parser# $ \env state token ->
      let (# s', res #) = cb env state token
       in case res of
            NoParseResult -> (# s', NoParseResult #)
            JustParseResult state r -> (# s', JustParseResult state (a `accSet` r) #)
  {-# INLINE (<$) #-}

instance (Semigroup err) => Applicative (Parser err) where
  pure = Parser . pureParser
  {-# INLINE pure #-}
  (Parser f) <*> (Parser a) = Parser (f `apParser` a)
  {-# INLINE (<*>) #-}
  {-# SPECIALIZE (<*>) :: Parser JSONError (a -> b) -> Parser JSONError a -> Parser JSONError b #-}
  {-# SPECIALIZE (<*>) :: Parser JSONObjectError (a -> b) -> Parser JSONObjectError a -> Parser JSONObjectError b #-}

instance (Monoid err) => Alternative (Parser err) where
  empty = Parser emptyParser
  {-# INLINE empty #-}
  (Parser l) <|> (Parser r) = Parser (l `altParser` r)
  {-# INLINE (<|>) #-}

instance (Semigroup err) => Monad (Parser err) where
  (Parser f) >>= cb = Parser (f `bindParser` (\res -> getParser (cb res)))
  {-# INLINE (>>=) #-}

parseBSIO :: Parser err res -> ByteString -> IO (Maybe (AE.AccumE err res))
parseBSIO (Parser (Parser# cb)) (PS (ForeignPtr addr contents) offset' len) = IO parse'
  where
    (I# endOffset) = offset' + len
    (I# startOffset) = offset'
    parse' s =
      let (# s', res #) = cb (InputRead contents addr endOffset) (InputState# startOffset) s
       in case res of
            NoParseResult -> (# s', Nothing #)
            JustParseResult _ (AccumEL err) -> (# s', Just (AE.AccumEL err) #)
            JustParseResult _ (AccumER res) -> (# s', Just (AE.AccumER res) #)

parseBS :: Parser err res -> ByteString -> Maybe (AE.AccumE err res)
parseBS parser bs = unsafeDupablePerformIO (parseBSIO parser bs)

currentOffset :: Parser err Int
currentOffset = Parser $
  Parser# $ \env i@(InputState# cs) s ->
    (# s, JustParseResult i (AccumER (I# cs)) #)
{-# INLINE currentOffset #-}

getEndOffset :: Parser err Int
getEndOffset = Parser $
  Parser# $ \env i s ->
    (# s, JustParseResult i (AccumER (I# (endOffset env))) #)
{-# INLINE getEndOffset #-}

parsedPtr :: Parser err (ForeignPtr Word8)
parsedPtr = Parser $
  Parser# $ \env i s -> (# s, JustParseResult i (AccumER $ ForeignPtr (addr env) (foreignPtr env)) #)

currentEnv :: Parser err (ForeignPtrContents, Ptr a, Int)
currentEnv = Parser $
  Parser# $ \env i s -> (# s, JustParseResult i (AccumER (foreignPtr env, Ptr (addr env), I# (endOffset env))) #)

failParse :: Parser err a
failParse = Parser $
  Parser# $ \env i s -> (# s, NoParseResult #)

maybeWord :: Parser err (Maybe Word8)
maybeWord = Parser $
  Parser# $ \env state@(InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, JustParseResult state (AccumER Nothing) #)
      else
        let (# s', word #) = readWord8OffAddr# (addr env) input s
         in (# s', JustParseResult (InputState# (input +# 1#)) (AccumER $ Just $ W8# word) #)

orFail :: Parser err (Maybe a) -> Parser err a
orFail (Parser (Parser# cb)) = Parser $
  Parser# $ \env s state ->
    let (# s', r #) = cb env s state
     in case r of
          NoParseResult -> (# s', NoParseResult #)
          JustParseResult state a ->
            case a of
              AccumER (Just a) -> (# s', JustParseResult state (AccumER a) #)
              _ -> (# s', NoParseResult #)

failWith :: err -> Parser err a
failWith err = Parser $
  Parser# $ \env s state -> (# state, JustParseResult s (AccumEL err) #)

asFailure :: Parser err err -> Parser err a
asFailure (Parser (Parser# cb)) = Parser $
  Parser# $ \env s state ->
    let (# s', r #) = cb env s state
     in case r of
          NoParseResult -> (# s', NoParseResult #)
          JustParseResult state a ->
            case a of
              AccumER a -> (# s', JustParseResult state (AccumEL a) #)
              AccumEL a -> (# s', JustParseResult state (AccumEL a) #)

-- | Lower a parsed error to a *parser error*.
lowerErr :: Parser err (Either err a) -> Parser err a
lowerErr (Parser (Parser# cb)) = Parser $
  Parser# $ \env s state ->
    let (# s', r #) = cb env s state
     in case r of
          NoParseResult -> (# s', NoParseResult #)
          JustParseResult state res ->
            case res of
              AccumER e -> (# s', JustParseResult state (eitherAcc e) #)
              AccumEL l -> (# s', JustParseResult state (AccumEL l) #)

-- | Do we have any further input?
hasFurther :: Parser err Bool
hasFurther = Parser $
  Parser# $ \env i@(InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, JustParseResult i (AccumER False) #)
      else (# s, JustParseResult i (AccumER True) #)

-- | Advance forward one word, fail if we can't
advanceWord :: Parser err ()
advanceWord = Parser $
  Parser# $ \env (InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, NoParseResult #)
      else (# s, JustParseResult (InputState# (input +# 1#)) (AccumER ()) #)

-- | Peek the next word, fail if there's nothing there
peekWord :: Parser err Word8
peekWord = Parser $
  Parser# $ \env i@(InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, NoParseResult #)
      else
        let (# s', word #) = readWord8OffAddr# (addr env) input s
         in (# s', JustParseResult i (AccumER $ W8# word) #)

-- | Peek a word, or nothing.
-- Never fails.
peekWordMaybe :: Parser err (Maybe Word8)
peekWordMaybe = Parser $
  Parser# $ \env i@(InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, JustParseResult i (AccumER Nothing) #)
      else
        let (# s', word #) = readWord8OffAddr# (addr env) input s
         in (# s', JustParseResult i (AccumER (Just (W8# word))) #)

word :: Parser err Word8
word = Parser $
  Parser# $ \env (InputState# input) s ->
    if isTrue# (input ==# endOffset env)
      then (# s, NoParseResult #)
      else
        let (# s', word #) = readWord8OffAddr# (addr env) input s
         in (# s', JustParseResult (InputState# (input +# 1#)) (AccumER $ W8# word) #)

specificWord w = orFail (cb <$> word)
  where
    cb w' =
      if w == w' then Just () else Nothing

-- | Skip over while the callback returns true.
--
-- Unlifted version, probably use skipWord8
skipWord8# :: (WordPrim -> Bool) -> Parser err ()
skipWord8# cb =
  Parser $
    Parser# (skipWord8CB# cb)
{-# INLINE skipWord8# #-}

-- | Skip over while the callback returns true
skipWord8 :: (Word8 -> Bool) -> Parser err ()
skipWord8 cb = skipWord8# (\byte -> cb (W8# byte {- HLINT ignore "Avoid lambda" -}))
{-# INLINE skipWord8 #-}

-- | Private: callback used for skipWord8
skipWord8CB# ::
  (WordPrim -> Bool) ->
  InputRead ->
  InputState ->
  State# RealWorld ->
  (# State# RealWorld, ParseResult# err () #)
skipWord8CB# cb env (InputState# input) s =
  let (# s', newOff #) = go input s
   in (# s', JustParseResult (InputState# newOff) (AccumER ()) #)
  where
    go :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    go inputOffset s =
      if isTrue# (inputOffset ==# endOffset env)
        then (# s, inputOffset #)
        else
          let (# s', word #) = readWord8OffAddr# (addr env) inputOffset s
           in if cb word then go (inputOffset +# 1#) s' else (# s', inputOffset #)

skipWhitespace :: Parser err ()
skipWhitespace = Parser $ Parser# skipWhitespaceCB
{-# INLINE skipWhitespace #-}

skipWhitespaceCB ::
  InputRead ->
  InputState ->
  State# RealWorld ->
  (# State# RealWorld, ParseResult# err () #)
skipWhitespaceCB env (InputState# input) s =
  let (# s', newOff #) = go input s
   in (# s', JustParseResult (InputState# newOff) (AccumER ()) #)
  where
    go :: Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
    go inputOffset s
      | isTrue# (inputOffset ==# endOffset env) = (# s, inputOffset #)
      | otherwise =
        let (# s', word #) = readWord8OffAddr# (addr env) inputOffset s
         in case W8# word of
              40 -> go (inputOffset +# 1#) s'
              0x20 -> go (inputOffset +# 1#) s'
              0x0A -> go (inputOffset +# 1#) s'
              0x0D -> go (inputOffset +# 1#) s'
              0x09 -> go (inputOffset +# 1#) s'
              _ -> (# s', inputOffset #)
    {-# INLINE go #-}
{-# INLINE skipWhitespaceCB #-}

signed :: (Monoid err, Num a) => Parser err a -> Parser err a
signed parser = withSign <|> parser
  where
    withSign = do
      r <- (specificWord 43 $> True) <|> (specificWord 45 $> False)
      if r
        then negate <$> parser
        else parser
{-# INLINE signed #-}

orNegative :: (Monoid err, Num a) => Parser err a -> Parser err a
orNegative parse =
  (specificWord 45 *> (negate <$> parse))
    <|> parse
{-# INLINE orNegative #-}

-- | Parse an integral number with possible leading zeros.
parseIntegral :: forall err i. (Monoid err, Integral i) => Parser err (Int, i)
parseIntegral = parseIntegralGo 0 0
{-# INLINE parseIntegral #-}

parseIntegralNoLeadingZero :: forall err i. (Monoid err, Integral i) => Parser err (Int, i)
parseIntegralNoLeadingZero = do
  w <- word
  if w >= 49 && w <= 57
    then parseIntegralGo 1 (fromIntegral $ w - 48)
    else failParse
{-# INLINE parseIntegralNoLeadingZero #-}

parseIntegralGo :: (Monoid err, Integral i) => Int -> i -> Parser err (Int, i)
parseIntegralGo digits acc = do
  r <- peekWordMaybe
  case r of
    Nothing -> pure (digits, acc)
    Just !w
      | w >= 48 && w <= 57 -> do
        let !accd = fromIntegral $ w - 48
        let !after = (acc * 10) + accd
        if after < 0 -- checks for overflow.
          then failParse
          else word *> parseIntegralGo (digits + 1) after
      | otherwise -> failParse
{-# SPECIALIZE parseIntegralGo :: (Monoid err) => Int -> Int -> Parser err (Int, Int) #-}
{-# SPECIALIZE parseIntegralGo :: (Monoid err) => Int -> Integer -> Parser err (Int, Integer) #-}

takeWord8Cont :: (Semigroup err) => (Word8 -> Bool) -> (BS.ByteString -> a) -> Parser err a
takeWord8Cont cb cont = do
  ptr <- parsedPtr
  offsetBefore <- currentOffset
  skipWord8 cb
  offsetAfter <- currentOffset
  pure $ cont (PS ptr offsetBefore (offsetAfter - offsetBefore))
{-# INLINE takeWord8Cont #-}

takeWord8 :: (Semigroup err) => (Word8 -> Bool) -> Parser err BS.ByteString
takeWord8 cb = takeWord8Cont cb id

takeWord81 :: (Semigroup err) => (Word8 -> Bool) -> Parser err BS.ByteString
takeWord81 cb = takeWord81Cont cb id

takeWord81Cont ::
  Semigroup err =>
  (Word8 -> Bool) ->
  (ByteString -> b) ->
  Parser err b
takeWord81Cont cb cont = do
  ptr <- parsedPtr
  offsetBefore <- currentOffset
  skipWord8 cb
  offsetAfter <- currentOffset
  let len = offsetAfter - offsetBefore
  when (len < 1) failParse
  pure $ cont (PS ptr offsetBefore len)

peekRest :: (Semigroup err) => Parser err BS.ByteString
peekRest = do
  ptr <- parsedPtr
  offset <- currentOffset
  es <- getEndOffset
  pure $ PS ptr offset (es - offset)

chunkOfLength :: Int -> Parser err BS.ByteString
chunkOfLength len@(I# len') = Parser $
  Parser# $ \env is@(InputState# off) s ->
    if
        | len < 0 -> (# s, NoParseResult #)
        | isTrue# (off +# len' ># endOffset env) -> (# s, NoParseResult #)
        | otherwise ->
          (#
            s,
            JustParseResult
              (InputState# (off +# len'))
              (AccumER $ PS (ForeignPtr (addr env) (foreignPtr env)) (I# off) len)
          #)

parseChunk :: ByteString -> Parser err ()
parseChunk chunk = orFail $ isChunkMaybe <$> chunkOfLength (BS.length chunk)
  where
    isChunkMaybe c = if chunk == c then Just () else Nothing

testParser :: Parser () (Word8, ByteString)
testParser = ((,) <$> word <*> takeWord8 (\c -> c >= 38 && c <= 57)) <|> ((,) <$> word <*> pure mempty)
