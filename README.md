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
type Language l = (Traversable l, ∀ a. Ord a => Ord (l a))
```

To declare a `Language` we must write the "base functor" of `SymExpr` (i.e. use
a type parameter where the recursion points used to be in the original
`SymExpr`), then instance `Traversable l`, `∀ a. Ord a => Ord (l a)` (we can do
it automatically through deriving), and write an `Analysis` instance for it (see
next section).

```hs
data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
               deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
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
Then, we define an `Analysis` for our `SymExpr`.

### Analysis

E-class analysis is first described in [*egg: Fast and Extensible Equality
Saturation*](https://arxiv.org/pdf/2004.03082.pdf) as a way to make equality
saturation more *extensible*.

With it, we can attach *analysis data* from a semilattice to each e-class. More
can be read about e-class analysis in the [`Data.Equality.Analsysis`]() module and
in the paper.

We can easily define constant folding (`2+2` being simplified to `4`) through
an `Analysis` instance.

An `Analysis` is defined over a `domain` and a `language`. To define constant
folding, we'll say the domain is `Maybe Double` to attach a value of that type to
each e-class, where `Nothing` indicates the e-class does not currently have a
constant value and `Just i` means the e-class has constant value `i`.

```hs
instance Analysis (Maybe Double) SymExpr
  makeA = ...
  joinA = ...
  modifyA = ...
```

Let's now understand and implement the three methods of the analysis instance we want.

`makeA` is called when a new e-node is added to a new e-class, and constructs
for the new e-class a new value of the domain to be associated with it, always
by accessing the associated data of the node's children data.  Its type is `l
domain -> domain`, so note that the e-node's children associated data is
directly available in place of the actual children.

We want to associate constant data to the e-class, so we must find if the
e-node has a constant value or otherwise return `Nothing`:

```hs
makeA :: SymExpr (Maybe Double) -> Maybe Double
makeA = \case
  Const x -> Just x
  Symbol _ -> Nothing
  x :+: y -> (+) <$> x <*> y
  x :*: y -> (*) <$> x <*> y
  x :/: y -> (/) <$> x <*> y
```
 
`joinA` is called when e-classes c1 c2 are being merged into c. In this case, we
must join the e-class data from both classes to form the e-class data to be
associated with new e-class c. Its type is `domain -> domain -> domain`.  In our
case, to merge `Just _` with `Nothing` we simply take the `Just`, and if we
merge two e-classes with a constant value (that is, both are `Just`), then the
constant value is the same (or something went very wrong) and we just keep it.

```hs
joinA :: Maybe Double -> Maybe Double -> Maybe Double
joinA Nothing (Just x) = Just x
joinA (Just x) Nothing = Just x
joinA Nothing Nothing  = Nothing
joinA (Just x) (Just y) = if x == y then Just x else error "ouch, that shouldn't have happened"
```

Finally, `modifyA` describes how an e-class should (optionally) be modified
according to the e-class data and what new language expressions are to be added
to the e-class also w.r.t. the e-class data.
Its type is `ClassId -> EGraph domain l -> EGraph domain l`, where the first argument
is the id of the class to modify (the class which prompted the modification),
and then receives and returns an e-graph, in which the e-class has been
modified.  For our example, if the e-class has a constant value associated to
it, we want to create a new e-class with that constant value and merge it to
this e-class.

```hs
-- import Data.Equality.Graph.Lens ((^.), _class, _data)
modifyA :: ClassId -> EGraph (Maybe Double) SymExpr -> EGraph (Maybe Double) SymExpr
modifyA c egr
    = case egr ^._class c._data of
        Nothing -> egr
        Just i ->
          let (c', egr') = represent (Fix (Const i)) egr
           in snd $ merge c c' egr'
```

Modify is a bit trickier than the other methods, but it allows our e-graph to
change based on the e-class analysis data. Note that the method is optional and
there's a default implementation for it which doesn't change the e-class or adds
anything to it. Analysis data can be otherwise used, e.g., to inform rewrite
conditions.

By instancing this e-class analysis, all e-classes that have a constant value
associated to them will also have an e-node with a constant value. This is great
for our simple symbolic library because it means if we ever find e.g. an
expression equal to `3+1`, we'll also know it to be equal to `4`, which is a
better result than `3+1` (we've then successfully implemented constant folding).

If, otherwise, we didn't want to use an analysis, we could specify the analysis
domain as `()` which will make the analysis do nothing, because there's an
instance polymorphic over `lang` for `()` that looks like this:

```hs
instance Analysis () lang where
  makeA _ = ()
  joinA _ _ = ()
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
type CostFunction l cost = l cost -> cost
```

A cost function is used to attribute a cost to representations in the e-graph and to extract the best one.
The first type parameter `l` is the language we're going to attribute a cost to, and
the second type parameter `cost` is the type with which we will model cost. For
the cost function to be valid, `cost` must instance `Ord`.

We'll say `Const`s and `Symbol`s are the cheapest and then in increasing cost we
have `:+:`, `:*:` and `:/:`, and model cost with the `Int` type.
```hs
cost :: CostFunction SymExpr Int
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

If we had instead `e2 = Fix (Fix (Fix (Symbol "x") :/: Fix (Symbol "x")) :+:
(Fix (Const 3))) -- (x/x)+3`, we'd get `expr = Const 4` because of our rewrite
rules put together with our constant folding!

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

## Debugging Rewrite Rules

To debug rewrite rules when doing equality saturation, wrap the `Scheduler`
with `TracingScheduler`. The tracing scheduler will use the underlying
scheduler but log all the rules matched beforehand. Seeing all the rules which
fire makes it easy to debug the set of rewrite rules, especially when it loops.

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

## Coverage

```
cabal test hegg-test --enable-coverage --enable-library-coverage
```

