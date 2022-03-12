{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a MergeMap, which is basically a map with
module Jordan.Types.Internal.MergeMap where

import qualified Data.Map as Map
import GHC.Exts (IsList (..))

-- | A map where '<>' merges values with '<>'.
newtype MergeMap key val = MergeMap {getMergeMap :: Map.Map key val}
  deriving (Eq)
  deriving (IsList) via (Map.Map key val)
  deriving (Functor, Foldable) via (Map.Map key)

instance Traversable (MergeMap key) where
  traverse f (MergeMap m) = MergeMap <$> traverse f m

instance (Semigroup val, Ord key) => Semigroup (MergeMap key val) where
  (MergeMap lhs) <> (MergeMap rhs) = MergeMap $ Map.unionWith (<>) lhs rhs
  {-# INLINE (<>) #-}

instance (Semigroup val, Ord key) => Monoid (MergeMap key val) where
  mempty = MergeMap mempty
  {-# INLINE mempty #-}

mergeSingleton :: k -> v -> MergeMap k v
mergeSingleton k v = MergeMap (Map.singleton k v)
