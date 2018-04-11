module Data.NonEmpty.Extras where

import Prelude

import Data.NonEmpty (NonEmpty, (:|))

hmap ::
     forall f g.
     (f ~> g)
  -> NonEmpty f
  ~> NonEmpty g
hmap f (a :| as) = (a :| f as)

map :: forall a b f. ((a -> b) -> f a -> f b) -> (a -> b) -> NonEmpty f a -> NonEmpty f b
map mapper f (a :| as) = (f a :| mapper f as)
