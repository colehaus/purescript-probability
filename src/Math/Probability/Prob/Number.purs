module Math.Probability.Prob.Number where

import Prelude

import Data.Monoid (class Monoid)

newtype Prob = MkProb Number
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
  top = MkProb 1.0
  bottom = MkProb 0.0
