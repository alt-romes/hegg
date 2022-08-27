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
represented in e-graph and on which equality saturation can be run:

```hs
class (Analysis l, Traversable l, Ord1 l) => Language l
```

To declare a `Language` we must write the "base functor" of `SymExpr` 
(i.e. use a type parameter where the recursion points used to be in the original `SymExpr`),
then instance `Traversable`, `Ord1`, and write an `Analysis` instance for it (see next section).

```hs
data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
               deriving (Functor, Foldable, Traversable)
infix 6 :+:
infix 7 :*:, :/:
```

Suggested reading on defining recursive data types in their parametrized
version: [Introduction To Recursion
Schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)

If we now wanted to represent an expression, we'd write it in its
fixed-point form

```hs
e1 :: Fix SymExpr
e1 = Fix (Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: (Fix (Const 2))) -- (x*2)/2
```

We've already automagically derived `Functor`, `Foldable` and `Traversable`
instances, and can use the following template haskell functions from `derive-compat` to derive `Ord1`.
```hs
deriveEq1   ''SymExpr
deriveOrd1  ''SymExpr
```

Then, we define an `Analysis` for our `SymExpr`.

### Analysis

E-class analysis is first described in [*egg: Fast and Extensible Equality
Saturation*](https://arxiv.org/pdf/2004.03082.pdf) as a way to make equality
saturation more *extensible*.

With it, we can attach *analysis data* from a semilattice to each e-class. More
can be read about e-class analysis in the [`Data.Equality.Analsysis`]() module and
in the paper.

We could easily define constant folding (`2+2` being simplified to `4`) through
an `Analysis` instance, but for the sake of simplicity we'll simply define the
analysis data as `()` and always ignore it.

```hs
instance Analysis SymExpr where
  type Domain SymExpr = ()
  makeA _ _ = ()
  joinA _ _ = ()
```

### Language, again

With this setup, we can now express that `SymExpr` forms a `Language` which we
can represent and manipulate in an e-graph by simply instancing it (there are no
additional functions to define).
```hs
instance Language SymExpr
```

### Equality saturation

Equality saturation is defined as the function
```hs
equalitySaturation :: forall l. Language l
                   => Fix l             -- ^ Expression to run equality saturation on
                   -> [Rewrite l]       -- ^ List of rewrite rules
                   -> CostFunction l    -- ^ Cost function to extract the best equivalent representation
                   -> (Fix l, EGraph l) -- ^ Best equivalent expression and resulting e-graph
```

To recap, our goal is to reach `x` starting from `(x*2)/2` by means of equality
saturation.

We already have a starting expression, so we're missing a list of rewrite rules
(`[Rewrite l]`) and a cost function (`CostFunction`).

### Cost function

Picking up the easy one first:
```hs
type CostFunction l = l Cost -> Cost
```

A cost function is used to attribute a cost to representations in the e-graph and to extract the best one.

We'll say `Const`s and `Symbol`s are the cheapest and then in increasing cost we
have `:+:`, `:*:` and `:/:`
```hs
cost :: CostFunction SymExpr
cost = \case
  Const  x -> 1
  Symbol x -> 1
  c1 :+: c2 -> c1 + c2 + 2
  c1 :*: c2 -> c1 + c2 + 3
  c1 :/: c2 -> c1 + c2 + 4
```

### Rewrite rules

Rewrite rules are transformations applied to matching expressions represented in
an e-graph.

We can write simple rewrite rules and conditional rewrite rules, but we'll only look at the simple ones.

A simple rewrite is formed of its left hand side and right hand side. When the
left hand side is matched in the e-graph, the right hand side is added to the
e-class where the left hand side was found.
```hs
data Rewrite lang = Pattern lang := Pattern lang          -- Simple rewrite rule
                  | Rewrite lang :| RewriteCondition lang -- Conditional rewrite rule
```

A `Pattern` is basically an expression that might contain variables and which can be matched against actual expressions.
```hs
data Pattern lang
    = NonVariablePattern (lang (Pattern lang))
    | VariablePattern Var
```
A patterns is defined by its non-variable and variable parts, and can be
constructed directly or using the helper function `pat` and using
`OverloadedStrings` for the variables, where `pat` is just a synonym for
`NonVariablePattern` and a string literal `"abc"` is turned into a `Pattern`
constructed with `VariablePattern`.

We can then write the following very specific set of rewrite rules to simplify
our simple symbolic expressions.
```hs
rewrites :: [Rewrite SymExpr]
rewrites =
  [ pat (pat ("a" :*: "b") :/: "c") := pat ("a" :*: pat ("b" :/: "c"))
  , pat ("x" :/: "x")               := pat (Const 1)
  , pat ("x" :*: (pat (Const 1)))   := "x"
  ]
```
### Equality saturation, again

We can now run equality saturation on our expression!

```hs
let expr = fst (equalitySaturation e1 rewrites cost)
```
And upon printing we'd see `expr = Symbol "x"`!

This was a first introduction which skipped over some details but that tried to
walk through fundamental concepts for using e-graphs and equality saturation
with this library.

The final code for this tutorial is available under `test/SimpleSym.hs`

A more complicated symbolic rewrite system which simplifies some derivatives and
integrals was written for the testsuite. It can be found at `test/Sym.hs`.

This library could also be used not only for equality-saturation but also for
the equality-graphs and other equality-things (such as e-matching) available.
For example, using just the e-graphs from `Data.Equality.Graph` to improve GHC's
pattern match checker (https://gitlab.haskell.org/ghc/ghc/-/issues/19272).

## Profiling

Notes on profiling for development.

For producing the info table, ghc-options must include `-finfo-table-map
-fdistinct-constructor-tables`

```
cabal run --enable-profiling hegg-test -- +RTS -p -s -hi -l-agu
ghc-prof-flamegraph hegg-test.prof
eventlog2html hegg-test.eventlog
open hegg-test.svg
open hegg-test.eventlog.html
```
