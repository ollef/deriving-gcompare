# deriving-gcompare

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
