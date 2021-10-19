{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module containing internal helpers for our parsers.
module Jordan.FromJSON.ParseInternal
    where

import Control.Applicative (Alternative(..))
import Data.Foldable (asum)

-- | A parser for permutations.
--
-- Based on the paper Parsing Permutation Phrases by
-- Arthur Baars, Andres Loh, and S. Doaitse Swierstra.
--
-- The source code for 'Control.Applicative.Permutations' really helped
-- in writing this, although this type is structured differently (and closer to the actual paper).
-- Thank you very much to Alex Washburn!
data Permutation parser a
  = Choice [Branch parser a]
  -- ^ We have multiple options for how to parse further.
  | Empty a
  -- ^ We have reached the end and only have a single value.

-- | A branch of a permutation.
-- Permutation parsers work by building up the entire tree of
-- possible parsers, which is efficient in Haskell due to laziness.
data Branch parser a
  = forall arg. Branch (Permutation parser (arg -> a)) (parser arg)

instance (Functor m) => Functor (Branch m) where
  fmap f (Branch perm p) = Branch (fmap (f .) perm) p

instance (Functor m) => Functor (Permutation m) where
  fmap f = \case
    Choice c -> Choice $ fmap f <$> c
    Empty a -> Empty (f a)

instance (Alternative m) => Applicative (Branch m) where
  pure a = Branch (pure $ const a) (pure ())
  (Branch permuteF argF) <*> (Branch permuteA argA) =
    Branch (args <$> permuteF <*> permuteA) arguments
      where
        arguments = ((,) <$> argA <*> argF) <|> (flip (,) <$> argF <*> argA)
        args :: (arg1 -> a -> b) -> (arg2 -> a) -> (arg2, arg1) -> b
        args f a (aa, fa) = f fa (a aa)

instance (Alternative m) => Applicative (Permutation m) where
  pure = Empty
  (Empty f) <*> (Empty a) = Empty $ f a
  (Empty f) <*> (Choice choices) = Choice $ fmap f <$> choices
  (Choice f) <*> (Empty a) = Choice $ fmap ($ a) <$> f
  t1@(Choice bs1) <*> t2@(Choice bs2) = Choice (map ins2 bs1 ++ map ins1 bs2)
    where
      ins1 (Branch perm p) = Branch ((.) <$> t1 <*> perm) p
      ins2 (Branch perm p) = Branch (flip <$> perm <*> t2) p

-- | Wrap up a permutation parser with two effects:
--
-- It will first interleave an infinite number of some effects, which represent parsing "junk" or unwanted fields.
-- At every stage of the permutation, we will first try to run the effect we want, and if it fails
-- we will try to run the "junk" effect instead, then try again.
--
-- We attempt to *intersperse* the second effect afterwards.
-- It adds a new effect between every effect.
-- This is used in parsing JSON to add commas.
wrapEffect
  :: forall m a b. (Alternative m)
  => m b
  -- ^ Consume a single, \"junk\" field.
  -- Used to ignore JSON keys that we do not care about.
  -> m b
  -- ^ Consume a \"separator\" between items in the permutation.
  -- This consumption is not done at the front of the permutation
  -- or after the end of it.
  -- This is used to parse commas between JSON fields.
  -> Permutation m a
  -- ^ The permutation parser to run.
  -> m a
  -- ^ The final parser.
wrapEffect takeSingle effAfter (Empty a) = pure a
wrapEffect takeSingle effAfter (Choice choices) = consumeMany
  where
    consumeMany
      = asum (pars <$> choices)
      -- Base case above: one of the choices of the permutation matched
      <|> (takeSingle *> effAfter *> consumeMany)
      -- Interleaving case: none of the choices of the permutation matched,
      -- so run a "junk" effect, the separator, and try again.
      -- Due to the recursion here we will do this infinitely until we either cannot
      -- run the junk effect, *or* we have a field that matches one of the choices of the permutation.
    runWithEffect :: Permutation m whatever -> m whatever
    runWithEffect (Empty a) = pure a
    runWithEffect (Choice choices) = effAfter *> consumeRec
        where
          consumeRec
            = asum (pars <$> choices)
            -- Run one of the effects from the permutation
            <|> (takeSingle *> effAfter *> consumeRec)
            -- Interleave a potentially infinite number of junk effects, with the separator effect between them.
    pars :: Branch m whatever -> m whatever
    pars (Branch perm arg) = do
      a <- arg
      rest <- runWithEffect perm
      pure $ rest a

asParser :: (Alternative f) => Permutation f a -> f a
asParser (Empty a) = pure a
asParser (Choice choices) = asum (pars <$> choices)
    where
      pars :: (Alternative f) => Branch f a -> f a
      pars (Branch perm arg) = do
        a <- arg
        rest <- asParser perm
        pure $ rest a

asPermutation :: (Alternative f) => f a -> Permutation f a
asPermutation p = Choice $ pure $ Branch (pure id) p
