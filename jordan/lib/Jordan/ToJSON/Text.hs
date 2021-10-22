{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jordan.ToJSON.Text
    where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.List (intersperse)
import qualified Data.Scientific as Sci
import Data.Semigroup (Endo(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Void (absurd)
import Jordan.ToJSON.Class

data TextComma
  = Written (T.Text -> T.Text)
  | Empty

runWritten :: TextComma -> T.Text -> T.Text
runWritten Empty = id
runWritten (Written f) = f

instance IsString TextComma where
  fromString s = Written (<> T.pack s)

instance Semigroup TextComma where
  Empty <> a = a
  a <> Empty = a
  (Written f) <> (Written f') = Written (f . (", " <>) . f')

instance Monoid TextComma where
  mempty = Empty

newtype CommaBuilder v = CommaBuilder { runCommaBuilder :: v -> TextComma }
  deriving (Semigroup, Monoid) via (v -> TextComma)

runCommaBuilder' :: CommaBuilder v -> v -> T.Text -> T.Text
runCommaBuilder' (CommaBuilder f) = runWritten . f

instance Contravariant CommaBuilder where
  contramap f (CommaBuilder v) = CommaBuilder (v . f)

instance Divisible CommaBuilder where
  conquer = CommaBuilder (const Empty)
  divide d (CommaBuilder b) (CommaBuilder c) = CommaBuilder $ \a ->
    let (b', c') = d a in b b' <> c c'

newtype TextArray v = TextArray { runTextArray :: v -> ([T.Text] -> [T.Text]) }
  deriving (Semigroup, Monoid) via (v -> Endo [T.Text])

instance Contravariant TextArray where
  contramap f (TextArray b) = TextArray $ \a -> b (f a)

instance Divisible TextArray where
  conquer = TextArray $ const mempty
  divide d (TextArray b) (TextArray c) = TextArray $ \a ->
    let (b', c') = d a in b b' . c c'

instance Decidable TextArray where
  lose _ = TextArray $ const mempty
  choose f (TextArray b) (TextArray c) = TextArray $ \a ->
    case f a of
      Left b'  -> b b'
      Right c' -> c c'

instance JSONTupleSerializer CommaBuilder where
  writeItem f = CommaBuilder $ Written . runJSONText f

instance JSONObjectSerializer CommaBuilder where
  writeField t s = CommaBuilder $ \arg ->
    Written (quoteString t . (": " <> ) . runJSONText s arg)

instance JSONTupleSerializer TextArray where
  writeItem f = TextArray $ \a -> ([runJSONText f a ""] <>)

newtype JSONText a
  = JSONText { runJSONText :: a -> (T.Text -> T.Text) }
  deriving (Semigroup, Monoid) via (a -> Endo T.Text)

instance Contravariant JSONText where
  contramap f (JSONText s) = JSONText (s . f)

instance Selectable JSONText where
  giveUp f = JSONText $ \a -> absurd (f a)
  select f (JSONText lhs) (JSONText rhs) = JSONText $ either lhs rhs . f

convChar :: Char -> (T.Text -> T.Text)
convChar = \case
  '\b' -> ("\\b" <>)
  '\f' -> ("\\f" <>)
  '\n' -> ("\\n" <>)
  '\r' -> ("\\r" <>)
  '\t' -> ("\\t" <>)
  '"' -> ("\\\"" <>)
  '\\' -> ("\\\\" <>)
  o -> (T.singleton o <>)

isBadChar :: Char -> Bool
isBadChar = \case
  '\b' -> True
  '\f' -> True
  '\r' -> True
  '\t' -> True
  '\\' -> True
  '"' -> True
  _ -> False

quoteString :: T.Text -> (T.Text -> T.Text)
quoteString t = ("\"" <>) . innerText . ("\"" <>)
  where
    innerText
      | T.any isBadChar t = T.foldl' (\o a -> o . convChar a) id t
      | otherwise = (t <>)

sArray :: (a -> T.Text -> T.Text) -> [a] -> (T.Text -> T.Text)
sArray _ [] = id
sArray f [x] = f x
sArray f (x : xs) = f x . ("," <>) . sArray f xs

instance JSONSerializer JSONText where
  serializeNull = JSONText $ const ("null" <>)
  serializeText = JSONText $ \a -> quoteString a
  serializeTextConstant t = JSONText $ const $ quoteString t
  serializeNumber = JSONText $ \n ->
    ((T.pack $ Sci.formatScientific Sci.Generic Nothing n) <>)
  serializeDictionary (JSONText serItem) = JSONText $ \n ->
    ("{" <>) . keys n . ("}" <>)
      where
        keys v = runWritten $ foldMap (\(k, v) -> Written $ quoteString k . (": " <> ) . serItem v) v
  serializeBool = JSONText $ \a -> ((if a then "true" else "false") <>)
  serializeObject n obj = JSONText $ \arg ->
    ("{" <>) . runCommaBuilder' obj arg . ("}" <>)
  serializeTuple obj = JSONText $ \arg ->
    ("[" <>) . runCommaBuilder' obj arg . ("}" <>)
  serializeArray = JSONText $ \a -> ("[" <>) . sArray (runJSONText toJSON) a . (<> "]")

toJSONText :: (ToJSON a) => a -> T.Text
toJSONText a = runJSONText toJSON a ""
