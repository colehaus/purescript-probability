module Math.Probability.Prob.Internal where

import Prelude

import Data.Monoid (class Monoid)
import Data.Rational (Rational, (%))

newtype Prob = MkProb Rational
derive newtype instance eqProb :: Eq Prob
derive newtype instance ordProb :: Ord Prob
derive newtype instance showProb :: Show Prob
instance semiProb :: Semigroup Prob where
  append (MkProb l) (MkProb r) = MkProb $ l * r
instance monoidProb :: Monoid Prob where
  mempty = top
instance boundedProb :: Bounded Prob where
  top = MkProb (1 % 1)
  bottom = MkProb (0 % 1)

addProb :: Prob -> Prob -> Prob
addProb (MkProb l) (MkProb r) = MkProb $ l + r
