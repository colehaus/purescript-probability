module Math.Probability.Prob.BigInt where

import Prelude

import Data.BigInt (BigInt, fromInt)
import Data.Monoid (class Monoid)
import Data.Ratio (Ratio, (%))

newtype Prob = MkProb (Ratio BigInt)
derive newtype instance eqProb :: Eq Prob
derive newtype instance ordProb :: Ord Prob
derive newtype instance showProb :: Show Prob
derive newtype instance semiringProb :: Semiring Prob
derive newtype instance ringProb :: Ring Prob
derive newtype instance commutativeProb :: CommutativeRing Prob
derive newtype instance euclideanProb :: EuclideanRing Prob
instance semiProb :: Semigroup Prob where
  append l r = l * r
instance monoidProb :: Monoid Prob where
  mempty = top
instance boundedProb :: Bounded Prob where
  top = MkProb (fromInt 1 % fromInt 1)
  bottom = MkProb (fromInt 0 % fromInt 1)
