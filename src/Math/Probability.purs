-- | Provides monadic interface for computation with discrete random variables.
-- | Based on: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf
module Math.Probability where

import Data.Array as A
import Control.Fold (mconcat, foldl)
import Data.Foldable as F
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Tuple (Tuple(..), fst, snd)
import Math (pow, sqrt)
import Prelude

import Math.Probability.Internal as P

type Prob = P.Prob

prob :: Number -> Maybe P.Prob
prob n | 0.0 P.<~ n && n P.<~ 1.0 = Just $ P.Prob n
prob n | otherwise = Nothing

runProb :: P.Prob -> Number
runProb (P.Prob p) = p

type ProbList = P.ProbList

probList :: Array P.Prob -> Maybe P.ProbList
probList ps =
  if F.sum (runProb <$> ps) P.~~ 1.0
  then Just $ P.ProbList ps
  else Nothing

runProbList :: P.ProbList -> Array P.Prob
runProbList (P.ProbList ps) = ps

type Dist = P.Dist

dist :: forall a. Array (Tuple a P.Prob) -> Maybe (P.Dist a)
dist d =
  if P.isValid d'
  then Just $ P.Dist d'
  else Nothing where
  d' = second runProb <$> d

zipDist :: forall a. Array a -> P.ProbList -> P.Dist a
zipDist as (P.ProbList ps) = P.Dist $ A.zipWith (\a (P.Prob p) -> Tuple a p) as ps

fromFreqs :: forall a. Array (Tuple a Number) -> Maybe (P.Dist a)
fromFreqs xs =
  let q = P.sumP xs in
  dist $ second (P.Prob <<< (_ / q)) <$> xs

choose :: forall a. P.Prob -> a -> a -> P.Dist a
choose p x y = let p' = runProb p in P.Dist [Tuple x p', Tuple y (1.0-p')]

runDist :: forall a. P.Dist a -> Array (Tuple a Number)
runDist (P.Dist a) = a

distProbs :: forall a. P.Dist a -> P.ProbList
distProbs (P.Dist a) = P.ProbList $ P.Prob <<< snd <$> a

extract :: forall a. P.Dist a -> Array a
extract = (<$>) fst <<< runDist

type Spread a = Array a -> Maybe (P.Dist a)

uniform :: forall a. Spread a
uniform = fromFreqs <<< (<$>) (\a -> Tuple a 1.0)

relative :: forall a. Array Number -> Spread a
relative ns = fromFreqs <<< flip A.zip ns

reshape :: forall a. Spread a -> P.Dist a -> Maybe (P.Dist a)
reshape s = s <<< extract

norm :: forall a. (Ord a) => P.Dist a -> P.Dist a
norm = P.lift P.norm'

type Event a = a -> Boolean

oneOf :: forall a. (Eq a) => Array a -> Event a
oneOf = flip F.elem

just :: forall a. (Eq a) => a -> Event a
just = (==)

infixr 1 lookup as ??
lookup :: forall a. Event a -> P.Dist a -> P.Prob
lookup p = P.Prob <<< P.sumP <<< A.filter (p <<< fst) <<< runDist

infixl 1 ffilter as >>=?
infixr 1 filter as ?=<<
ffilter :: forall a. P.Dist a -> Event a -> Maybe (P.Dist a)
ffilter = flip filter
filter :: forall a. Event a -> P.Dist a -> Maybe (P.Dist a)
filter p = fromFreqs <<< A.filter (p <<< fst) <<< runDist

cond :: forall a. P.Dist Boolean -> P.Dist a -> P.Dist a -> P.Dist a
cond b d d' = b >>= \c -> if c then d else d'

joinDists :: forall a b c. (a -> b -> c) -> P.Dist a -> (a -> P.Dist b) -> P.Dist c
joinDists f as bs'a = do
  a <- as
  b <- bs'a a
  pure $ f a b

marginalize :: forall a b. (Eq b) => (a -> b) -> P.Dist a -> P.Dist b
marginalize f d =
  P.Dist <<< (<$>) (\b -> Tuple b <<< runProb $ ((==) b <<< f) ?? d) <<<
  A.nub $ f <$> extract d

data Iso a b = Iso (a -> b) (b -> a)
to :: forall a b. Iso a b -> a -> b
to (Iso f _) = f
from :: forall a b. Iso a b -> b -> a
from (Iso _ f) = f

expected :: forall a. Iso a Number -> P.Dist a -> a
expected i = from i <<< F.sum <<< (<$>) (\(Tuple a b) -> to i a * b) <<< runDist

variance :: forall a. Iso a Number -> P.Dist a -> a
variance i xs =
  from i <<< expected i' $ (\x -> pow (x - m) 2.0) <$> xs' where
    i' = (Iso id id)
    m = expected i' xs'
    xs' = to i <$> xs

stdDev :: forall a. Iso a Number -> P.Dist a -> a
stdDev i = from i <<< sqrt <<< to i <<< variance i

approx :: forall a. (Ord a) => P.Dist a -> P.Dist a -> Boolean
approx (P.Dist xs) (P.Dist ys) =
  unwrap <<< foldl mconcat $
  A.zipWith (\(Tuple x p) (Tuple y q) -> Conj $ x == y && p P.~~ q) xs ys

size :: forall a. P.Dist a -> Number
size = I.toNumber <<< A.length <<< runDist

map :: forall a b. (Ord b) => (a -> b) -> P.Dist a -> P.Dist b
map f = norm <<< (<$>) f

mapMaybe :: forall a b. (a -> Maybe b) -> P.Dist a -> Maybe (P.Dist b)
mapMaybe f =
  fromFreqs <<< A.mapMaybe (\(Tuple a p) -> flip Tuple p <$> f a) <<< runDist

