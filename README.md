## hegg

Draft: Fast equality saturation in Haskell

Based on [*egg: Fast and Extensible Equality Saturation*](https://arxiv.org/pdf/2004.03082.pdf), [*Relational E-matching*](https://arxiv.org/pdf/2108.02290.pdf) and the [rust implementation](https://github.com/egraphs-good/egg).

### Profiling

There are some cost centers manually set with `{-# SCC f #-}`

Run and generate the `.prof` file and `svg` from it
```
cabal run --enable-profiling hegg-test -- +RTS -p

ghc-prof-flamegraph hegg-test.prof

open hegg-test.svg
```

Running profiling on a slow integral shows that 55-50% of the time is spent on the
ill-named `intersectAtoms`
