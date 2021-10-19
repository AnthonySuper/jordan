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
import qualified Data.Text as T
import Data.Void (absurd)
import Jordan.ToJSON.Class

newtype TextObject a
  = TextObject { runTextObject :: a -> (T.Text -> T.Text) }
  deriving (Semigroup, Monoid) via (a -> Endo T.Text)

instance Contravariant TextObject where
  contramap f (TextObject s) = TextObject (s . f)

instance Divisible TextObject where
  conquer = TextObject $ const mempty
  divide div (TextObject sb) (TextObject sc) = TextObject $ \arg ->
    let (b, c) = div arg in
        sb b . (", " <>) . sc c

instance Decidable TextObject where
  lose f = TextObject $ \a -> absurd (f a)
  choose f (TextObject sb) (TextObject sc) = TextObject $ \arg ->
    case f arg of
      Left b  -> sb b
      Right c -> sc c

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

instance JSONTupleSerializer TextArray where
  writeItem f = TextArray $ \a -> ([runJSONText f a ""] <>)

instance JSONObjectSerializer TextObject where
  writeField t s = TextObject $ \arg ->
    quoteString t . (": " <>) . runJSONText s arg

instance JSONTupleSerializer TextObject where
  writeItem f = TextObject $ \a -> runJSONText f a

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
  serializeBool = JSONText $ \a -> ((if a then "true" else "false") <>)
  serializeObject n obj = JSONText $ \arg ->
    ("{" <>) . runTextObject obj arg . ("}" <>)
  serializeTuple obj = JSONText $ \a ->
    let ran = runTextArray obj a [] in
        (("[" <> mconcat (intersperse ", " ran) <> "]") <>)
  serializeArray = JSONText $ \a -> ("[" <>) . sArray (runJSONText toJSON) a . (<> "]")

toJSONText :: (ToJSON a) => a -> T.Text
toJSONText a = runJSONText toJSON a ""
