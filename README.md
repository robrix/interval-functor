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


## Development

Development currently assumes a Mac with `ghc` 8.10 & `cabal` 3.0. You can install them directly, or use [`ghcup`](https://www.haskell.org/ghcup/). It should be possible to develop on other platforms and compilers, but I probably haven’t tried them myself.

```bash
cabal build --enable-tests # initial build
script/repl # load the library and tests in ghci
```

Once the repl has loaded, you can run the tests with `:main`.

```
λ :main
```

Configuration exists for `ghcide`, which can be integrated into many editors.


## Advantages

`interval-functor` separates the representation of the coordinates of an interval from the representation of the space the coordinates occur in. This makes it particularly suitable for consistent treatment of multi-dimensional intervals; operations like e.g. `union` extend naturally to multi-dimensional spaces.
