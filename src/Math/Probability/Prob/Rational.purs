module Math.Probability.Prob.Rational where

import Prelude

import Data.Newtype (class Newtype)
import Data.Generic.Rep (class Generic)
import Data.Rational (Rational, (%))

newtype Prob = MkProb Rational
derive instance genericProb :: Generic Prob _
derive instance newtypeProb :: Newtype Prob _
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
  top = MkProb (1 % 1)
  bottom = MkProb (0 % 1)
