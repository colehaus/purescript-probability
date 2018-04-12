module Math.Probability.Dist.Internal where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Tuple (Tuple(..))

newtype Dist p a = MkDist (List (Tuple a p))
derive newtype instance eqDist :: (Eq a, Eq p) => Eq (Dist p a)
derive newtype instance ordDist :: (Ord a, Ord p) => Ord (Dist p a)
derive newtype instance showDist :: (Show a, Show p) => Show (Dist p a)

instance functorDist :: Functor (Dist p) where
  map f (MkDist d) = MkDist $ first f <$> d
    where
      first g (Tuple a b) = Tuple (g a) b

instance applyDist :: Semigroup p => Apply (Dist p) where
  apply (MkDist d) a = MkDist do
    Tuple f p <- d
    Tuple b q <- (\(MkDist t) -> t) a
    pure $ Tuple (f b) (p <> q)

instance applicativeDist :: (Bounded p, Semigroup p) => Applicative (Dist p) where
  pure x = MkDist <<< List.singleton $ Tuple x top

instance bindDist :: Semigroup p => Bind (Dist p) where
  bind (MkDist d) f = MkDist $ do
    Tuple a p <- d
    Tuple b q <- (\(MkDist t) -> t) $ f a
    pure $ Tuple b (p <> q)

instance monadDist :: (Bounded p, Semigroup p) => Monad (Dist p)

lift :: forall a p. (List (Tuple a p) -> List (Tuple a p)) -> Dist p a -> Dist p a
lift f (MkDist a) = MkDist $ f a

unDist :: forall a p. Dist p a -> List (Tuple a p)
unDist (MkDist d) = d
