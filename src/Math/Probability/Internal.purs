-- | Data declarations which are hidden behind smart constructors in `Probability`.
module Math.Probability.Internal where

import Data.Array as A
import Data.Foldable as F
import Data.Function (on)
import Data.Map as M
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst, snd)
import Math (abs)
import Prelude

newtype Prob = Prob Number
newtype ProbList = ProbList (Array Prob)
newtype Dist a = Dist (Array (Tuple a Number))

epsilon :: Number
epsilon = 0.000005

infix 4 approxEq as ~~
approxEq :: Number -> Number -> Boolean
approxEq a b = abs (a - b) < epsilon
infix 4 approxNeq as /~
approxNeq :: Number -> Number -> Boolean
approxNeq a b = not $ a ~~ b
infix 4 approxLt as <~
approxLt :: Number -> Number -> Boolean
approxLt a b = a - b < epsilon
infix 4 approxGt as >~
approxGt :: Number -> Number -> Boolean
approxGt a b = b - a < epsilon

lift :: forall a. (Array (Tuple a Number) -> Array (Tuple a Number)) -> Dist a -> Dist a
lift f (Dist a) = Dist $ f a

sumP :: forall a. Array (Tuple a Number) -> Number
sumP = F.sum <<< (<$>) snd

sortP :: forall a. Array (Tuple a Number) -> Array (Tuple a Number)
sortP = A.sortBy (compare `on` snd)

sortElem :: forall a. (Ord a) => Array (Tuple a Number) -> Array (Tuple a Number)
sortElem = A.sortBy (compare `on` fst)

norm' :: forall a. (Ord a) => Array (Tuple a Number) -> Array (Tuple a Number)
norm' = M.toUnfoldable <<< M.fromFoldableWith (+)

isValid :: forall a. Array (Tuple a Number) -> Boolean
isValid = (~~) 1.0 <<< sumP

instance functorDist :: Functor Dist where
  map f (Dist d) = Dist $ first f <$> d

instance applyDist :: Apply Dist where
  apply (Dist d) a = Dist $ do
    (Tuple f p) <- d
    (Tuple b q) <- (\(Dist t) -> t) a
    pure $ Tuple (f b) (p * q)

instance applicativeDist :: Applicative Dist where
  pure x = Dist <<< A.singleton $ Tuple x 1.0

instance bindDist :: Bind Dist where
  bind (Dist d) f = Dist $ do
    (Tuple a p) <- d
    (Tuple b q) <- (\(Dist t) -> t) $ f a
    pure $ Tuple b (p * q)

instance monadDist :: Monad Dist

derive newtype instance eqProb :: Eq Prob
derive newtype instance ordProb :: Ord Prob
derive newtype instance showProb :: Show Prob

derive newtype instance eqDist :: (Eq a) => Eq (Dist a)
derive newtype instance ordDist :: (Ord a) => Ord (Dist a)
derive newtype instance showDist :: (Show a) => Show (Dist a)

derive newtype instance eqProbList :: Eq ProbList
derive newtype instance ordProbList :: Ord ProbList
derive newtype instance showProbList :: Show ProbList

