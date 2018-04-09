module Math.Probability.Dist where

import Prelude

import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.List (List(Nil, Cons))
import Data.List.Unique as Unique
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Indexed as Indexed
import Data.Profunctor.Strong (second)
import Data.Ratio (Ratio)
import Data.Tuple (Tuple, fst, snd)
import Math.Probability.Dist.Internal (Dist(..), unDist)
import Math.Probability.Prob (Prob)
import Math.Probability.Prob as Prob
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)

make ::
     forall a. Ord a
  => Indexed.NonEmpty Map a Prob
  -> Dist a
make d =
  MkDist <<<
  map (second (\x -> fromJustNote <<< Prob.make $ (Prob.unmake x / q))) $
  unfolded
  where
    fromJustNote x =
      unsafePartialBecause
        "`Prob` over sum of `Probs` should always be between 0 and 1" $
      fromJust x
    q = sum unfolded
    unfolded = Map.toUnfoldable <<< Indexed.fromNonEmpty Map.insert $ d

isValid ::
  forall p f a.
     Foldable f
  => Functor f
  => Eq p
  => f (Tuple a Prob)
  -> Boolean
isValid = eq (Prob.unmake top) <<< sum

sum ::
  forall f a.
      Foldable f
   => Functor f
   => f (Tuple a Prob)
   -> Ratio Int
sum = Foldable.sum <<< map (Prob.unmake <<< snd)

unmake ::
  forall a.
    Ord a
  => Dist a
  -> Indexed.NonEmpty Map a Prob
unmake d =
  case unDist d of
    Cons a tail -> a Indexed.:| Map.fromFoldable tail
    Nil -> unsafeCrashWith "A `Dist` should always have at least one element"

probs ::
     forall a. Ord a
  => Dist a
  -> NonEmpty List Prob
probs = Indexed.deindex snd Map.values <<< unmake

values ::
     forall a.
     Ord a
  => Dist a
  -> NonEmpty Unique.List a
values d = Indexed.deindex fst (toUniqueNoted <<< Map.keys) <<< unmake $ d
  where
    toUniqueNoted x =
      unsafePartialBecause "`Map` keys are necessarily unique" $
      fromRight <<< Unique.fromFoldable $ x
