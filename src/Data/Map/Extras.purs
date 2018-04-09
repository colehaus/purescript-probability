module Data.Map.Extras where

import Prelude

import Control.Monad.State (execState, modify)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)

mapMaybe ::
     forall k v u.
     Ord k
  => (v -> Maybe u)
  -> Map k v
  -> Map k u
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey ::
     forall k v u.
     Ord k
  => (k -> v -> Maybe u)
  -> Map k v
  -> Map k u
mapMaybeWithKey f map = execState (traverseWithIndex f' map) mempty
  where
    f' k v = maybe (pure unit) (modify <<< insert k) $ f k v

mapKeys ::
     forall j k v.
     Ord j
  => Ord k
  => (k -> j)
  -> Map k v
  -> Map j v
mapKeys f = mapKeysMaybeWithValueWith (\k v _ -> Just (Tuple (f k) v))

mapKeysMaybe ::
     forall j k v.
     Ord j
  => Ord k
  => (k -> Maybe j)
  -> Map k v
  -> Map j v
mapKeysMaybe f = mapKeysMaybeWithValueWith (\k v _ -> (_ `Tuple` v) <$> f k)

mapKeysWith ::
     forall j k v.
     Ord j
  => Ord k
  => (k -> j)
  -> (v -> v -> v)
  -> Map k v
  -> Map j v
mapKeysWith f g = mapKeysMaybeWithValueWith f'
  where
    f' k v (Just v') = Just (Tuple (f k) (g v v'))
    f' k v Nothing = Just (Tuple (f k) v)

mapKeysMaybeWithValueWith ::
     forall j k v.
     Ord j
  => Ord k
  => (k -> v -> Maybe v -> Maybe (Tuple j v))
  -> Map k v
  -> Map j v
mapKeysMaybeWithValueWith f map = execState (traverseWithIndex f' map) mempty
  where
    f' k v =
      maybe (pure unit) (modify <<< uncurry insert) $ f k v (k `lookup` map)
