# Improvement to the sort in the haskell base library

To get the amount of comparisons, add `comparisons` to the `bgroup` in `benchmark`.
To run with allocations, add `+RTS -T`


[GHC Merge Request](https://gitlab.haskell.org/ghc/ghc/-/issues/24280)

[CLC proposal](https://github.com/haskell/core-libraries-committee/issues/236)
