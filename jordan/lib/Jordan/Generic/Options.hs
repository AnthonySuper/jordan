{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jordan.Generic.Options where

import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Bool
import Data.Typeable (TypeRep, splitTyConApp, tyConModule, tyConName)
import GHC.Exts (Constraint)
import GHC.Generics
import GHC.TypeLits

type Representational (f :: * -> *) =
  (forall a b. (Coercible a b) => Coercible (f a) (f b) :: Constraint)

data SumTypeEncoding
  = TagVal
  | TagInField
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

type family AllNullary cons where
  AllNullary (C1 ('MetaCons _ _ 'False) (S1 ('MetaSel 'Nothing _ _ _) U1)) = True
  AllNullary (a :+: b) = AllNullary a && AllNullary b
  AllNullary _ = False

newtype PartOfSum f a = MkPartOfSum {getPartOfSum :: f a}
  deriving (Show, Read, Eq, Ord, Generic)

-- | A newtype wrapper, designed to make it easier to derive ToJSON and FromJSON instances.
-- The API of abstract JSON serializing is awkward due to the somewhat bad ergonomics of the
-- 'Data.Functor.Contravariant.Divisible.Divisible' and (especially)
-- 'Data.Functor.Contravariant.Divisible.Decidable' typeclasses.
--
-- In general, using @ -XDerivingVia @, @ -XDeriveGeneric @, @ -XDataKinds @ and this wrapper will make your life much easier.
-- You can specify things like so:
--
-- @
--  data PersonFilter = PersonFilter { filterFirstName :: Maybe Text, filterLastName :: Maybe Text }
--    deriving (Show, Generic)
--    deriving (ToJSON, FromJSON) via (WithOptions '[KeepNothingFields] PersonFilter)
-- @
newtype WithOptions (options :: [*]) a = WithOptions {getWithOptions :: a}
  deriving (Show, Eq, Ord)

-- | Newtype for use with GeneralizedNewtypeDeriving.
-- Will have us omit Nothing fields for parsing and serializing.
data OmitNothingFields = OmitNothingFields

-- | Keep nothing fields.
-- Will have us omit @ null @ when serializing Maybe types.
data KeepNothingFields = KeepNothingFields

fullyQualifyName ::
  TypeRep ->
  Text
fullyQualifyName tr =
  case splitTyConApp tr of
    (tc, []) -> baseName tc
    (tc, args) -> baseName tc <> "(" <> T.intercalate "," (fullyQualifyName <$> args) <> ")"
  where
    baseName tc = T.pack (tyConModule tc <> "." <> tyConName tc)
