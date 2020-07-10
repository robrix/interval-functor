# `interval-functor`

This is a Haskell package defining an `Interval` datatype parameterized by the type of endpoints and the type of their coordinates. For example, it can represent a simple one-dimensional range:

```haskell
import Data.Functor.Identity
import Data.Functor.Interval

zeroInterval :: Num a => Interval Identity a
zeroInterval = point 0
```

or a multi-dimensional region:

```haskell
import Data.Functor.Interval
import Linear.V3 -- from the linear package

unitCube :: Num a => Interval V3 a
unitCube = (-1)...1
```
