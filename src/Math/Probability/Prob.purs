module Math.Probability.Prob
  ( module Math.Probability.Prob
  , module ForReExport
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Rational (Rational)

import Math.Probability.Prob.Internal (Prob(..))
import Math.Probability.Prob.Internal (Prob) as ForReExport

make :: Rational -> Maybe Prob
make r =
  if isValid r
    then Just (MkProb r)
    else Nothing

isValid :: Rational -> Boolean
isValid r = bottom <= MkProb r && MkProb r <= top

unmake :: Prob -> Rational
unmake (MkProb r) = r
