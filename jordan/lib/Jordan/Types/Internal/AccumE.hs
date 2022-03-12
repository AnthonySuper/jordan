{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Either, but with an Applicative instance that combines errors via '<>'.
--
-- This is sometimes known as the validation Applicative.
-- There are Haskell packages providing this type, however, in the interest of minimized
-- dependency footprint we use this.
module Jordan.Types.Internal.AccumE
  ( AccumE
      ( AccumE,
        getAccumE,
        AccumEL,
        AccumER
      ),
  )
where

import Control.Applicative
import Data.Bifunctor
import GHC.Generics
import Text.Read

-- | A version of Either that accumulates errors via an instance of 'Semigroup'.
--
-- This is sometimes called the validation applicative.
newtype AccumE err val = AccumE {getAccumE :: Either err val}
  deriving (Functor) via Either err
  deriving (Bifunctor) via Either
  deriving (Generic)

-- | Show instance uses the 'AccumER' and 'AccumEL' pattern synonyms.
instance (Show a, Show b) => Show (AccumE a b) where
  showsPrec prec = \case
    AccumEL l ->
      showParen (prec > 10) $
        showString "AccumEL " . showsPrec 11 l
    AccumER r ->
      showParen (prec > 10) $
        showString "AccumER " . showsPrec 11 r

-- | Read instance uses the 'AccumER' and 'AccumEL' pattern synonyms.
instance (Read a, Read b) => Read (AccumE a b) where
  readPrec = parens $ left +++ right
    where
      left = do
        Ident "AccumEL" <- lexP
        AccumEL <$> step readPrec
      right = do
        Ident "AccumER" <- lexP
        AccumER <$> step readPrec

-- | Construct an error value.
pattern AccumEL :: err -> AccumE err val
pattern AccumEL l = AccumE (Left l)

-- | Construct a good value.
-- Equivalent to 'pure'.
pattern AccumER :: val -> AccumE err val
pattern AccumER r = AccumE (Right r)

{-# COMPLETE AccumEL, AccumER #-}

-- | Applicative accumulates errors.
--
-- Note that this is *strict* in the error, because this
-- can sometimes reduce the number of allocations in the places
-- where we use this.
instance (Semigroup e) => Applicative (AccumE e) where
  pure !a = AccumE (Right a)
  {-# INLINE pure #-}
  (AccumE !f) <*> (AccumE !a) = AccumE $ case f of
    Left !e -> case a of
      Left !e' -> Left $ e <> e'
      Right !a' -> Left e
    Right !fab -> case a of
      Left !e -> Left e
      Right !arg -> Right $ fab arg
  {-# INLINE (<*>) #-}
  liftA2 f (AccumE arg) (AccumE arg') =
    AccumE
      ( case arg of
          Left e -> case arg' of
            Left e' -> Left $ e <> e'
            Right b -> Left e
          Right a -> case arg' of
            Left e -> Left e
            Right b -> Right (f a b)
      )
  {-# INLINE liftA2 #-}

-- | Alternative takes the first result if there is a result.
-- If there is not, will *not* accumulate errors.
instance (Monoid m) => Alternative (AccumE m) where
  empty = AccumEL mempty
  (AccumER a) <|> _ = AccumER a
  (AccumEL _) <|> (AccumER a) = AccumER a
  (AccumEL a) <|> (AccumEL _) = AccumEL a
  {-# INLINE (<|>) #-}

-- | Semigroup accumulates errors if both are errors, otherwise
-- it returns the first good value.
instance (Semigroup e) => Semigroup (AccumE e a) where
  (AccumE lhs) <> (AccumE rhs) = AccumE $ case lhs of
    Left e -> case rhs of
      Left e' -> Left $ e <> e'
      Right a -> Right a
    Right a -> Right a

-- mempty is an error with 'mempty'
instance (Monoid err) => Monoid (AccumE err a) where
  mempty = AccumEL mempty
