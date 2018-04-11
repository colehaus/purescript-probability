-- | Provides monadic interface for computation with discrete random variables.
-- | Based on: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
module Math.Probability
  ( module Math.Probability
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.Int (round)
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extras (mapKeysMaybe, mapKeysWith)
import Data.Map.Extras as Map
import Data.Maybe (Maybe(Nothing, Just))
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Extras as NonEmpty
import Data.NonEmpty.Indexed (index)
import Data.NonEmpty.Indexed as Indexed
import Data.Rational (Rational, toNumber, (%))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Math (pow, sqrt)
import Partial.Unsafe (unsafeCrashWith)

import Math.Probability.Dist as Dist
import Math.Probability.Dist.Internal (Dist(..))
import Math.Probability.Dist.Internal as Dist
import Math.Probability.Prob as Prob
import Math.Probability.Prob.Internal (Prob(..))
import Math.Probability.Prob.Internal as Prob


complement :: Prob -> Prob
complement (MkProb p) = MkProb $ (1 % 1) - p

choose :: forall a. Prob -> a -> a -> Dist a
choose p x y = MkDist $ Tuple x p : Tuple y (complement p) : Nil

type Spread a = NonEmpty Set a -> Dist a

uniform ::
     forall a.
     Ord a
  => Spread a
uniform =
  Dist.make <<<
  Indexed.index (_ `Tuple` top) (Map.fromFoldable <<< Set.map (_ `Tuple` top))

relative ::
     forall a. Ord a
  => (a -> Prob)
  -> Spread a
relative f =
  Dist.make <<<
  Indexed.index (Tuple <*> f) (Map.fromFoldable <<< Set.map (Tuple <*> f))

focus ::
     forall a.
     Ord a
  => a
  -> Spread a
focus special =
  Dist.make <<<
  index id Map.fromFoldable <<< NonEmpty.map Set.map (apply Tuple prob)
  where
    prob a =
      if a == special
        then top
        else bottom

reshape ::
     forall a. Ord a
  => Spread a
  -> Dist a
  -> Dist a
reshape s = s <<< Dist.values

norm ::
     forall a. Ord a
  => Dist a
  -> Dist a
norm = Dist.lift (Map.toUnfoldable <<< Map.fromFoldableWith Prob.add)

type Event a = a -> Boolean

oneOf :: forall a. (Eq a) => List a -> Event a
oneOf = flip Foldable.elem

just :: forall a. (Eq a) => a -> Event a
just = (==)

infixr 1 lookup as ??
lookup ::
     forall a.
     Ord a
  => Event a
  -> Dist a
  -> Prob
lookup p =
  fromJustNoted <<<
  Prob.make <<<
  Dist.sum <<<
  asList <<< Map.toUnfoldable <<< Map.filterKeys p <<< toMap <<< Dist.unmake
  where
    fromJustNoted (Just a) = a
    fromJustNoted Nothing =
      unsafeCrashWith
        "Any individual event in a `Dist` should have a proper probability"
    asList :: List (Tuple a Prob) -> List (Tuple a Prob)
    asList = id

infixl 1 ffilter as >>=?
infixr 1 filter as ?=<<
ffilter :: forall a. Ord a => Dist a -> Event a -> Maybe (Dist a)
ffilter = flip filter
filter ::
     forall a.
     Ord a
  => Event a
  -> Dist a
  -> Maybe (Dist a)
filter p d =
  (\({ key, value }) -> Dist.make $ (Tuple key value) Indexed.:| key `Map.delete` m) <$>
  Map.findMin m
  where
    m =
      Map.filterKeys p <<< toMap <<< Dist.unmake $ d

cond :: forall a. Dist Boolean -> Dist a -> Dist a -> Dist a
cond b d d' = b >>= \c -> if c then d else d'

joinDists :: forall a b c. (a -> b -> c) -> Dist a -> (a -> Dist b) -> Dist c
joinDists f as bs'a = do
  a <- as
  b <- bs'a a
  pure $ f a b

marginalize ::
     forall a b.
     Ord a
  => Ord b
  => (a -> b)
  -> Dist a
  -> Dist b
marginalize f = lift $ Indexed.reindex f (mapKeysWith Prob.add f)

type Iso a b = { to :: (a -> b), from :: (b -> a) }

expected ::
     forall a.
     Ord a
  => Iso a Rational
  -> Dist a
  -> a
expected i =
  i.from <<<
  Foldable.sum <<<
  asList <<<
  (<$>) (\(Tuple a b) -> i.to a * Prob.unmake b) <<<
  Map.toUnfoldable <<< toMap <<< Dist.unmake
  where
    asList :: List Rational -> List Rational
    asList = id

variance :: forall a. Iso a Rational -> Dist a -> a
variance i xs =
  i.from <<< expected i' $
  (\x -> numToRational $ pow (toNumber $ x - m) 2.0) <$> xs'
  where
    numToRational n = (round $ n * 10e6) % round 10e6
    i' = { to: id, from: id }
    m = expected i' xs'
    xs' = i.to <$> xs

stdDev :: forall a. Iso a Rational -> Dist a -> a
stdDev i = i.from <<< numToRational <<< sqrt <<< toNumber <<< i.to <<< variance i
  where
    numToRational n = (round $ n * 10e6) % round 10e6

size ::
     forall a. Ord a
  => Dist a
  -> Int
size = Map.size <<< toMap <<< Dist.unmake

map ::
     forall a b. Ord b
  => (a -> b)
  -> Dist a
  -> Dist b
map f = norm <<< (<$>) f

mapMaybe ::
     forall a b.
     Ord a
  => Ord b
  => (a -> Maybe b)
  -> Dist a
  -> Maybe (Dist b)
mapMaybe f dist =
  (\({ key, value }) -> Dist.make $ Tuple key value Indexed.:| key `Map.delete` m) <$>
  Map.findMin m
  where
    m =
      mapKeysMaybe f <<< toMap <<< Dist.unmake $ dist

toMap ::
     forall v k.
     Ord k
  => Indexed.NonEmpty Map k Prob
  -> Map k Prob
toMap = Indexed.fromNonEmpty (Map.insertWith Prob.add)

lift ::
     forall a b.
     Ord a
  => Ord b
  => (Indexed.NonEmpty Map a Prob -> Indexed.NonEmpty Map b Prob)
  -> Dist a
  -> Dist b
lift f = Dist.make <<< f <<< Dist.unmake
