-- | Provides monadic interface for computation with discrete random variables.
-- | Based on: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
module Math.Probability
  ( module Math.Probability
  ) where

import Prelude

import Data.Foldable as Foldable
import Data.List (List(Nil), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extras (mapKeysMaybe, mapKeysWith)
import Data.Map.Extras as Map
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty.Extras as NonEmpty
import Data.NonEmpty.Indexed (index)
import Data.NonEmpty.Indexed as Indexed
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(Tuple))
import Math (sqrt)

import Math.Probability.Dist as Dist
import Math.Probability.Dist.Internal (Dist(..))
import Math.Probability.Dist.Internal as Dist


complement :: forall p. Bounded p => Ring p => p -> p
complement p = top - p

choose ::
     forall a p.
     Bounded p
  => Ring p
  => p
  -> a
  -> a
  -> Dist p a
choose p x y = MkDist $ Tuple x p : Tuple y (complement p) : Nil

type Spread p a = NonEmpty Set a -> Dist p a

uniform ::
     forall a p.
     Bounded p
  => EuclideanRing p
  => Ord a
  => Spread p a
uniform =
  Dist.make <<<
  Indexed.index (_ `Tuple` top) (Map.fromFoldable <<< Set.map (_ `Tuple` top))

relative ::
     forall a p.
     EuclideanRing p
  => Ord a
  => (a -> p)
  -> Spread p a
relative f =
  Dist.make <<<
  Indexed.index
    (Tuple <*> f)
    (Map.fromFoldable <<< (<$>) (Tuple <*> f) <<< asList <<< Set.toUnfoldable)
  where
    asList :: forall b. List b -> List b
    asList = id

focus ::
     forall a p.
     Bounded p
  => EuclideanRing p
  => Ord a
  => a
  -> Spread p a
focus special =
  Dist.make <<<
  index id Map.fromFoldable <<< NonEmpty.map Set.map (apply Tuple prob)
  where
    prob a | a == special = top
           | otherwise = bottom

reshape ::
     forall a p.
     Ord a
  => Semiring p
  => Spread p a
  -> Dist p a
  -> Dist p a
reshape s = s <<< Dist.values

norm ::
     forall a p.
     Semiring p
  => Ord a
  => Dist p a
  -> Dist p a
norm = Dist.lift (Map.toUnfoldable <<< Map.fromFoldableWith (+))

type Event a = a -> Boolean

oneOf :: forall a. (Eq a) => List a -> Event a
oneOf = flip Foldable.elem

just :: forall a. (Eq a) => a -> Event a
just = (==)

infixr 1 lookup as ??
lookup ::
     forall a p.
     Ord a
  => Semiring p
  => Event a
  -> Dist p a
  -> p
lookup p =
  Dist.sum <<<
  asList <<< Map.toUnfoldable <<< Map.filterKeys p <<< toMap <<< Dist.unmake
  where
    asList :: forall b. List b -> List b
    asList = id

infixl 1 ffilter as >>=?
infixr 1 filter as ?=<<
ffilter ::
     forall a p.
     Ord a
  => EuclideanRing p
  => Semiring p
  => Dist p a
  -> Event a
  -> Maybe (Dist p a)
ffilter = flip filter
filter ::
     forall a p.
     Ord a
  => EuclideanRing p
  => Semiring p
  => Event a
  -> Dist p a
  -> Maybe (Dist p a)
filter p d =
  (\({ key, value }) -> Dist.make $ (Tuple key value) Indexed.:| key `Map.delete` m) <$>
  Map.findMin m
  where
    m =
      Map.filterKeys p <<< toMap <<< Dist.unmake $ d

cond ::
     forall a p.
     Semigroup p
  => Dist p Boolean
  -> Dist p a
  -> Dist p a
  -> Dist p a
cond b d d' = b >>= \c -> if c then d else d'

joinDists ::
     forall a b c p.
     Bounded p
  => Semigroup p
  => (a -> b -> c)
  -> Dist p a
  -> (a -> Dist p b)
  -> Dist p c
joinDists f as bs'a = do
  a <- as
  b <- bs'a a
  pure $ f a b

marginalize ::
     forall a b p.
     Ord a
  => Ord b
  => EuclideanRing p
  => (a -> b)
  -> Dist p a
  -> Dist p b
marginalize f = lift $ Indexed.reindex f (mapKeysWith (+) f)

type Iso a b = { to :: (a -> b), from :: (b -> a) }

expected ::
     forall a p.
     Ord a
  => Semiring a
  => Semiring p
  => (p -> a)
  -> Dist p a
  -> a
expected project =
  Foldable.sum <<<
  asList <<<
  (<$>) (\(Tuple a p) -> a * project p) <<<
  Map.toUnfoldable <<< toMap <<< Dist.unmake
  where
    asList :: forall b. List b -> List b
    asList = id

variance :: forall a p. Ord a => Ring a => Semiring p => (p -> a) -> Dist p a -> a
variance project xs =
  expected project $
  (\x -> (x - m) * (x - m)) <$> xs
  where
    m = expected project xs

stdDev ::
     forall a p.
     Ord a
  => Ring a
  => Semiring p
  => Iso a Number
  -> (p -> a)
  -> Dist p a -> a
stdDev i project = i.from <<< sqrt <<< i.to <<< variance project

size ::
     forall a p.
     Ord a
  => Semiring p
  => Dist p a
  -> Int
size = Map.size <<< toMap <<< Dist.unmake

map ::
     forall a b p.
     Ord b
  => Semiring p
  => (a -> b)
  -> Dist p a
  -> Dist p b
map f = norm <<< (<$>) f

mapMaybe ::
     forall a b p.
     Ord a
  => Ord b
  => Semiring p
  => EuclideanRing p
  => (a -> Maybe b)
  -> Dist p a
  -> Maybe (Dist p b)
mapMaybe f dist =
  (\({ key, value }) -> Dist.make $ Tuple key value Indexed.:| key `Map.delete` m) <$>
  Map.findMin m
  where
    m =
      mapKeysMaybe f <<< toMap <<< Dist.unmake $ dist

toMap ::
     forall p k.
     Ord k
  => Semiring p
  => Indexed.NonEmpty Map k p
  -> Map k p
toMap = Indexed.fromNonEmpty (Map.insertWith (+))

lift ::
     forall a b p.
     EuclideanRing p
  => Ord a
  => Ord b
  => (Indexed.NonEmpty Map a p -> Indexed.NonEmpty Map b p)
  -> Dist p a
  -> Dist p b
lift f = Dist.make <<< f <<< Dist.unmake
