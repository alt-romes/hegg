## hegg

Fast equality saturation in Haskell

Based on [*egg: Fast and Extensible Equality Saturation*](https://arxiv.org/pdf/2004.03082.pdf), [*Relational E-matching*](https://arxiv.org/pdf/2108.02290.pdf) and the [rust implementation](https://github.com/egraphs-good/egg).

### Equality Saturation and E-graphs

Suggested material on equality saturation and e-graphs for beginners
* (tutorial) https://docs.rs/egg/latest/egg/tutorials/_01_background/index.html
* (5m video) https://www.youtube.com/watch?v=ap29SzDAzP0

## Equality saturation in Haskell

To get a feel for how we can use `hegg` and do equality saturation in Haskell,
we'll write a simple numeric *symbolic* manipulation library that can simplify expressions
according to a set of rewrite rules by leveraging equality saturation.

If you've never heard of symbolic mathematics you might get some intuition from
reading [Let’s Program a Calculus
Student](https://iagoleal.com/posts/calculus-symbolic/) first.

### Syntax

We'll start by defining the abstract syntax tree for our simple symbolic expressions:
```hs
data SymExpr = Const Double
             | Symbol String
             | SymExpr :+: SymExpr
             | SymExpr :*: SymExpr
             | SymExpr :/: SymExpr
infix 6 :+:
infix 7 :*:, :/:

e1 :: SymExpr
e1 = (Symbol "x" :*: Const 2) :/: (Const 2) -- (x*2)/2
```

You might notice that `(x*2)/2` is the same as just `x`. Our goal is to get
equality saturation to do that for us.

Our second step is to instance `Language` for our `SymExpr`

### Language

`Language` is the required constraint on *expressions* that are to be
represented in e-graph and on which equality saturation can be run.

```hs
class (Analysis l, Traversable l, Ord1 l) => Language l
```

To form a `Language`, we must define our `SymExpr` in its functor form, then
instance `Traversable`, `Ord1`, and define an `Analysis` on it.

Suggested reading on defining recursive data types in their parametrized version
* [Introduction To Recursion Schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)

By adding a type parameter to our `SymExpr` we get
```hs
data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
infix 6 :+:
infix 7 :*:, :/:
```

And if we now wanted to represent an expression, we'd write it in its
fixed-point form
```hs
e1 :: Fix SymExpr
e1 = Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: (Fix (Const 2)) -- (x*2)/2
```

## Profiling

For producing the info table, ghc-options must include `-finfo-table-map
-fdistinct-constructor-tables`

```
cabal run --enable-profiling hegg-test -- +RTS -p -s -hi -l-agu
ghc-prof-flamegraph hegg-test.prof
eventlog2html hegg-test.eventlog
open hegg-test.svg
open hegg-test.eventlog.html
```
