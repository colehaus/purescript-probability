# Module Documentation

## Module Math.Probability


Provides monadic interface for computation with discrete random variables.
Based on: http://web.engr.oregonstate.edu/~erwig/papers/PFP_JFP06.pdf

#### `ProbList`

``` purescript
type ProbList = P.ProbList
```


#### `Prob`

``` purescript
type Prob = P.Prob
```


#### `Dist`

``` purescript
type Dist = P.Dist
```


#### `Event`

``` purescript
type Event a = a -> Boolean
```


#### `Spread`

``` purescript
type Spread a = [a] -> Maybe (P.Dist a)
```


#### `oneOf`

``` purescript
oneOf :: forall a. (Eq a) => [a] -> Event a
```


#### `just`

``` purescript
just :: forall a. (Eq a) => a -> Event a
```


#### `dist`

``` purescript
dist :: forall a. [Tuple a P.Prob] -> Maybe (P.Dist a)
```


#### `runDist`

``` purescript
runDist :: forall a. P.Dist a -> [Tuple a Number]
```


#### `distProbs`

``` purescript
distProbs :: forall a. P.Dist a -> P.ProbList
```


#### `extract`

``` purescript
extract :: forall a. P.Dist a -> [a]
```


#### `approx`

``` purescript
approx :: forall a. (Ord a) => P.Dist a -> P.Dist a -> Boolean
```


#### `size`

``` purescript
size :: forall a. P.Dist a -> Number
```


#### `isValid`

``` purescript
isValid :: forall a. [Tuple a Number] -> Boolean
```


#### `zipDist`

``` purescript
zipDist :: forall a. [a] -> P.ProbList -> P.Dist a
```


#### `fromFreqs`

``` purescript
fromFreqs :: forall a. [Tuple a Number] -> Maybe (P.Dist a)
```


#### `norm`

``` purescript
norm :: forall a. (Ord a) => P.Dist a -> P.Dist a
```


#### `choose`

``` purescript
choose :: forall a. P.Prob -> a -> a -> P.Dist a
```


#### `reshape`

``` purescript
reshape :: forall a. Spread a -> P.Dist a -> Maybe (P.Dist a)
```


#### `relative`

``` purescript
relative :: forall a. [Number] -> Spread a
```


#### `uniform`

``` purescript
uniform :: forall a. Spread a
```


#### `map`

``` purescript
map :: forall a b. (Ord b) => (a -> b) -> P.Dist a -> P.Dist b
```


#### `cond`

``` purescript
cond :: forall a. P.Dist Boolean -> P.Dist a -> P.Dist a -> P.Dist a
```


#### `(?=<<)`

``` purescript
(?=<<) :: forall a. (a -> Boolean) -> P.Dist a -> Maybe (P.Dist a)
```


#### `(>>=?)`

``` purescript
(>>=?) :: forall a. P.Dist a -> (a -> Boolean) -> Maybe (P.Dist a)
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> P.Dist a -> Maybe (P.Dist a)
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> P.Dist a -> Maybe (P.Dist b)
```


#### `runProb`

``` purescript
runProb :: P.Prob -> Number
```


#### `prob`

``` purescript
prob :: Number -> Maybe P.Prob
```


#### `(??)`

``` purescript
(??) :: forall a. Event a -> P.Dist a -> P.Prob
```


#### `joinDists`

``` purescript
joinDists :: forall a b c. (a -> b -> c) -> P.Dist a -> (a -> P.Dist b) -> P.Dist c
```


#### `marginalize`

``` purescript
marginalize :: forall a b. (Eq b) => (a -> b) -> P.Dist a -> P.Dist b
```


#### `Iso`

``` purescript
data Iso a b
  = Iso (a -> b) (b -> a)
```


#### `to`

``` purescript
to :: forall a b. Iso a b -> a -> b
```


#### `from`

``` purescript
from :: forall a b. Iso a b -> b -> a
```


#### `expected`

``` purescript
expected :: forall a. Iso a Number -> P.Dist a -> a
```


#### `variance`

``` purescript
variance :: forall a. Iso a Number -> P.Dist a -> a
```


#### `stdDev`

``` purescript
stdDev :: forall a. Iso a Number -> P.Dist a -> a
```


#### `probList`

``` purescript
probList :: [P.Prob] -> Maybe P.ProbList
```


#### `runProbList`

``` purescript
runProbList :: P.ProbList -> [P.Prob]
```



## Module Math.Probability.Internal


Data declarations which are hidden behind smart constructors in `Probability`.

#### `Prob`

``` purescript
newtype Prob
  = Prob Number
```


#### `ProbList`

``` purescript
newtype ProbList
  = ProbList [Prob]
```


#### `Dist`

``` purescript
newtype Dist a
  = Dist [Tuple a Number]
```


#### `epsilon`

``` purescript
epsilon :: Number
```


#### `(~~)`

``` purescript
(~~) :: Number -> Number -> Boolean
```


#### `(/~)`

``` purescript
(/~) :: Number -> Number -> Boolean
```


#### `(<~)`

``` purescript
(<~) :: Number -> Number -> Boolean
```


#### `(>~)`

``` purescript
(>~) :: Number -> Number -> Boolean
```


#### `lift`

``` purescript
lift :: forall a. ([Tuple a Number] -> [Tuple a Number]) -> Dist a -> Dist a
```


#### `sumP`

``` purescript
sumP :: forall a. [Tuple a Number] -> Number
```


#### `sortP`

``` purescript
sortP :: forall a. [Tuple a Number] -> [Tuple a Number]
```


#### `sortElem`

``` purescript
sortElem :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
```


#### `norm'`

``` purescript
norm' :: forall a. (Ord a) => [Tuple a Number] -> [Tuple a Number]
```


#### `functorDist`

``` purescript
instance functorDist :: Functor Dist
```


#### `applyDist`

``` purescript
instance applyDist :: Apply Dist
```


#### `applicativeDist`

``` purescript
instance applicativeDist :: Applicative Dist
```


#### `bindDist`

``` purescript
instance bindDist :: Bind Dist
```


#### `monadDist`

``` purescript
instance monadDist :: Monad Dist
```


#### `eqProb`

``` purescript
instance eqProb :: Eq Prob
```

#### `ordProb`

``` purescript
instance ordProb :: Ord Prob
```


#### `showProb`

``` purescript
instance showProb :: Show Prob
```


#### `eqDist`

``` purescript
instance eqDist :: (Eq a) => Eq (Dist a)
```


#### `ordDist`

``` purescript
instance ordDist :: (Ord a) => Ord (Dist a)
```


#### `showDist`

``` purescript
instance showDist :: (Show a) => Show (Dist a)
```


#### `eqProbList`

``` purescript
instance eqProbList :: Eq ProbList
```


#### `ordProbList`

``` purescript
instance ordProbList :: Ord ProbList
```


#### `showProbList`

``` purescript
instance showProbList :: Show ProbList
```