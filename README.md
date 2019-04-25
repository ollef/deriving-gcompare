# deriving-gcompare

You should probably be using
[dependent-sum-template](http://hackage.haskell.org/package/dependent-sum-template),
which I didn't realise existed when I wrote this, instead.

Usage:

```haskell
import Data.GADT.Compare.Deriving

data Q a where
  A :: Q Int
  B :: Int -> Q String
  C :: Bool -> Int -> Q Bool

deriveGEq ''Q
deriveGCompare ''Q
```
