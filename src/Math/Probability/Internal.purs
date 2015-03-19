-- | Data declarations which are hidden behind smart constructors in `Probability`.
module Math.Probability.Internal where

import Control.Arrow
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Function
import qualified Data.Map as M
import Data.Profunctor.Strong
import Data.Tuple
import Math

newtype Prob = Prob Number
newtype ProbList = ProbList [Prob]
newtype Dist a = Dist [Tuple a Number]

epsilon :: Number
epsilon = 0.000005

infix 4 ~~
(~~) :: Number -> Number -> Boolean
(~~) a b = abs (a - b) < epsilon
infix 4 /~
(/~) :: Number -> Number -> Boolean
(/~) a b = not $ a ~~ b
infix 4 <~
(<~) :: Number -> Number -> Boolean
(<~) a b = a - b < epsilon
infix 4 >~
(>~) :: Number -> Number -> Boolean
(>~) a b = b - a < epsilon

lift :: forall a. ([Tuple a Number] -> [Tuple a Number]) -> Dist a -> Dist a
lift f (Dist a) = Dist $ f a

sumP :: forall a. [Tuple a Number] -> Number
sumP = F.sum <<< (<$>) snd

sortP :: forall a. [Tuple a Number] -> [Tuple a Number]
sortP = A.sortBy (compare `on` snd)

sortElem :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
sortElem = A.sortBy (compare `on` fst)

norm' :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
norm' = M.toList <<< M.fromListWith (+)

isValid :: forall a. [Tuple a Number] -> Boolean
isValid = (~~) 1 <<< sumP

instance functorDist :: Functor Dist where
  (<$>) f (Dist d) = Dist $ first f <$> d

instance applyDist :: Apply Dist where
  (<*>) (Dist d) a = Dist $ do
    (Tuple f p) <- d
    (Tuple b q) <- (\(Dist t) -> t) a
    pure $ Tuple (f b) (p * q)

instance applicativeDist :: Applicative Dist where
  pure x = Dist <<< A.singleton $ Tuple x 1

instance bindDist :: Bind Dist where
  (>>=) (Dist d) f = Dist $ do
    (Tuple a p) <- d
    (Tuple b q) <- (\(Dist t) -> t) $ f a
    pure $ Tuple b (p * q)

instance monadDist :: Monad Dist

-- Derivable boilerplate

instance eqProb :: Eq Prob where
  (==) (Prob a) (Prob b) = a == b
  (/=) a b = not $ a == b
instance ordProb :: Ord Prob where
  compare (Prob a) (Prob b) = compare a b
instance showProb :: Show Prob where
  show (Prob a) = show a

instance eqDist :: (Eq a) => Eq (Dist a) where
  (==) (Dist a) (Dist b) = a == b
  (/=) a b = not $ a == b
instance ordDist :: (Ord a) => Ord (Dist a) where
  compare (Dist a) (Dist b) = a `compare` b
instance showDist :: (Show a) => Show (Dist a) where
  show (Dist a) = "Dist " <> show a

instance eqProbList :: Eq ProbList where
  (==) (ProbList a) (ProbList b) = a == b
  (/=) a b = not $ a == b
instance ordProbList :: Ord ProbList where
  compare (ProbList a) (ProbList b) = a `compare` b
instance showProbList :: Show ProbList where
  show (ProbList a) = "ProbList (" <> show a <> ")"

