module Data.Map.Extras where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..), uncurry)

mapMaybe :: forall k v u. Ord k => (v -> Maybe u) -> Map k v -> Map k u
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: forall k v u. Ord k => (k -> v -> Maybe u) -> Map k v -> Map k u
mapMaybeWithKey f m = foldlWithIndex f' mempty m
  where
    f' k acc v = maybe acc (flip (insert k) acc) $ f k v

mapKeys :: forall j k v. Ord j => Ord k => (k -> j) -> Map k v -> Map j v
mapKeys f = mapKeysMaybeWithValueWith (Just <<< f) (\k v _ -> Just (Tuple (f k) v))

mapKeysMaybe :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> Map k v -> Map j v
mapKeysMaybe f = mapKeysMaybeWithValueWith f (\k v _ -> (_ `Tuple` v) <$> f k)

-- | The value at the greater of the two original keys is used as the first argument to c.
mapKeysWith :: forall j k v. Ord j => Ord k => (v -> v -> v) -> (k -> j) -> Map k v -> Map j v
mapKeysWith f g = mapKeysMaybeWithValueWith (Just <<< g) h
  where
    h k v (Just v') = Just (Tuple (g k) (f v v'))
    h k v Nothing = Just (Tuple (g k) v)

mapKeysMaybeWithValueWith :: forall j k v. Ord j => Ord k => (k -> Maybe j) -> (k -> v -> Maybe v -> Maybe (Tuple j v)) -> Map k v -> Map j v
mapKeysMaybeWithValueWith f g m = foldlWithIndex h mempty m
  where
    h k acc v = maybe acc (flip (uncurry insert) acc) $ g k v <<< (flip lookup acc) =<< f k
