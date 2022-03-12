{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Module containing internal helpers for our parsers.
module Jordan.FromJSON.Internal.Permutation where

import Control.Applicative (Alternative (..))
import Control.Monad (void, when)
import Data.Bifunctor
import Data.Foldable (asum)
import Data.Functor.Compose
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace

data FailingParser parser
  = FailingParser (forall a. parser a)
  | NoFailingParser

instance (Applicative parser) => Semigroup (FailingParser parser) where
  (FailingParser a) <> (FailingParser b) = FailingParser (a *> b)
  (FailingParser a) <> NoFailingParser = FailingParser a
  NoFailingParser <> (FailingParser a) = FailingParser a
  NoFailingParser <> NoFailingParser = NoFailingParser

eliminateFailing :: (Alternative parser) => FailingParser parser -> parser a
eliminateFailing (FailingParser f) = f
eliminateFailing NoFailingParser = empty

type role Permutation nominal representational

-- | A parser for permutations.
--
-- Based on the paper Parsing Permutation Phrases by
-- Arthur Baars, Andres Loh, and S. Doaitse Swierstra.
--
-- The source code for 'Control.Applicative.Permutations' really helped
-- in writing this, although this type is structured differently (and closer to the actual paper).
-- Thank you very much to Alex Washburn!
data Permutation parser a
  = Permutation !(Maybe a) !(FailingParser parser) [Branch parser a]

type role Branch nominal representational

-- | A branch of a permutation.
-- Permutation parsers work by building up the entire tree of
-- possible parsers, which is efficient in Haskell due to laziness.
data Branch parser a
  = forall arg. Branch (Permutation parser (arg -> a)) (parser arg)

instance (Functor m) => Functor (Branch m) where
  fmap f (Branch perm p) = Branch (fmap (f .) perm) p

instance (Functor m) => Functor (Permutation m) where
  fmap f (Permutation def failing branches) =
    Permutation (f <$> def) failing (fmap f <$> branches)

instance (Alternative m) => Applicative (Branch m) where
  pure a = Branch (pure $ const a) (pure ())
  (Branch permuteF argF) <*> (Branch permuteA argA) =
    Branch (args <$> permuteF <*> permuteA) arguments
    where
      arguments = ((,) <$> argA <*> argF) <|> (flip (,) <$> argF <*> argA)
      args :: (arg1 -> a -> b) -> (arg2 -> a) -> (arg2, arg1) -> b
      args f a (aa, fa) = f fa (a aa)

instance (Alternative m) => Applicative (Permutation m) where
  pure val = Permutation (Just val) NoFailingParser empty

  t1@(Permutation defF failingF choiceF) <*> t2@(Permutation defA failingA choiceA) =
    Permutation (defF <*> defA) NoFailingParser (map ins2 choiceF ++ map ins1 choiceA)
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
wrapEffect ::
  forall m a b.
  (Alternative m) =>
  -- | Consume a single, \"junk\" field.
  -- Used to ignore JSON keys that we do not care about.
  m b ->
  -- | Consume a \"separator\" between items in the permutation.
  -- This consumption is not done at the front of the permutation
  -- or after the end of it.
  -- This is used to parse commas between JSON fields.
  m b ->
  -- | The permutation parser to run.
  Permutation m a ->
  -- | The final parser.
  m a
wrapEffect takeSingle effAfter (Permutation def failing choices) = consumeMany
  where
    consumeMany =
      foldr ((<|>) . pars) empty choices
        -- Base case above: one of the choices of the permutation matched
        <|> (takeSingle *> effAfter *> consumeMany)
        <|> maybe empty pure def

    -- Interleaving case: none of the choices of the permutation matched,
    -- so run a "junk" effect, the separator, and try again.
    -- Due to the recursion here we will do this infinitely until we either cannot
    -- run the junk effect, *or* we have a field that matches one of the choices of the permutation.
    runWithEffect :: Permutation m whatever -> m whatever
    runWithEffect (Permutation def failing choices) = (effAfter *> consumeRec) <|> maybe (eliminateFailing failing) pure def
      where
        consumeRec =
          foldr ((<|>) . pars) empty choices
            -- Run one of the effects from the permutation
            <|> (takeSingle *> effAfter *> consumeRec)
    -- Interleave a potentially infinite number of junk effects, with the separator effect between them.
    pars :: Branch m whatever -> m whatever
    pars (Branch perm arg) = do
      a <- arg
      rest <- runWithEffect perm
      pure $ rest a

asParser :: (Alternative f) => Permutation f a -> f a
asParser (Permutation def failing choices) = asum (pars <$> choices) <|> maybe empty pure def <|> eliminateFailing failing
  where
    pars :: (Alternative f) => Branch f a -> f a
    pars (Branch perm arg) = do
      a <- arg
      rest <- asParser perm
      pure $ rest a

asPermutation :: (Alternative f) => f a -> Permutation f a
asPermutation p = Permutation Nothing NoFailingParser $ pure $ Branch (pure id) p

asPermutationWithDefault :: (Alternative f) => f a -> a -> Permutation f a
asPermutationWithDefault per def = Permutation (Just def) NoFailingParser $ pure $ Branch (pure id) per

asPermutationWithFailing :: (Alternative f) => f a -> (forall b. f b) -> Permutation f a
asPermutationWithFailing parse fail = Permutation Nothing (FailingParser fail) $ pure $ Branch (pure id) parse

asPermutationWithDefaultFailing :: (Alternative f) => f a -> (forall b. f b) -> a -> Permutation f a
asPermutationWithDefaultFailing parse fail def = Permutation (Just def) (FailingParser fail) $ pure $ Branch (pure id) parse
