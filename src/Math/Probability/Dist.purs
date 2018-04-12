module Math.Probability.Dist
  ( module Math.Probability.Dist
  , module ForReExport
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.List (List(Nil, Cons))
import Data.Map (Map)
import Data.Map as Map
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafeCrashWith)

import Math.Probability.Dist.Internal (Dist(..), unDist)
import Math.Probability.Dist.Internal (Dist) as ForReExport


make ::
     forall a p.
     Ord a
  => EuclideanRing p
  => Indexed.NonEmpty Map a p
  -> Dist p a
make d =
  MkDist <<<
  map (second (_ / q)) $
  unfolded
  where
    second f (Tuple a b) = Tuple a $ f b
    q = sum unfolded
    unfolded = Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert $ d

isValid ::
  forall p f a.
     Bounded p
  => Semiring p
  => Foldable f
  => Functor f
  => Eq p
  => f (Tuple a p)
  -> Boolean
isValid = eq top <<< sum

sum ::
  forall f a p.
      Semiring p
   => Foldable f
   => Functor f
   => f (Tuple a p)
   -> p
sum = Foldable.sum <<< map snd

unmake ::
  forall a p.
     Ord a
  => Semiring p
  => Dist p a
  -> Indexed.NonEmpty Map a p
unmake d =
  case unDist d of
    Cons a tail -> a Indexed.:| Map.fromFoldableWith (+) tail
    Nil -> unsafeCrashWith "A `Dist` should always have at least one element"

probs ::
     forall a p.
     Ord a
  => Semiring p
  => Dist p a
  -> NonEmpty List p
probs = Indexed.deindex snd Map.values <<< unmake

values ::
     forall a p.
     Ord a
  => Semiring p
  => Dist p a
  -> NonEmpty Set a
values = Indexed.deindex fst (Set.fromFoldable <<< Map.keys) <<< unmake
