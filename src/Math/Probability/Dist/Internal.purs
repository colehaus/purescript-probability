module Math.Probability.Dist.Internal where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))
import Math.Probability.Prob.Internal (Prob)

newtype Dist a = MkDist (List (Tuple a Prob))
derive newtype instance eqDist :: (Eq a) => Eq (Dist a)
derive newtype instance ordDist :: (Ord a) => Ord (Dist a)
derive newtype instance showDist :: (Show a) => Show (Dist a)

instance functorDist :: Functor Dist where
  map f (MkDist d) = MkDist $ first f <$> d

instance applyDist :: Apply Dist where
  apply (MkDist d) a = MkDist do
    Tuple f p <- d
    Tuple b q <- (\(MkDist t) -> t) a
    pure $ Tuple (f b) (p <> q)

instance applicativeDist :: Applicative Dist where
  pure x = MkDist <<< List.singleton $ Tuple x top

instance bindDist :: Bind Dist where
  bind (MkDist d) f = MkDist $ do
    Tuple a p <- d
    Tuple b q <- (\(MkDist t) -> t) $ f a
    pure $ Tuple b (p <> q)

instance monadDist :: Monad Dist

lift :: forall a. (List (Tuple a Prob) -> List (Tuple a Prob)) -> Dist a -> Dist a
lift f (MkDist a) = MkDist $ f a

unDist :: forall a. Dist a -> List (Tuple a Prob)
unDist (MkDist d) = d
