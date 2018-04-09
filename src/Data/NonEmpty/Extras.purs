module Data.NonEmpty.Extras where

import Prelude

import Data.NonEmpty (NonEmpty, (:|))

hmap ::
     forall f g.
     (f ~> g)
  -> NonEmpty f
  ~> NonEmpty g
hmap f (a :| as) = (a :| f as)
