# Computed rewrite patterns

`ComputePattern lhs build` extends computed rewrites with RHS patterns that
retain the matched e-classes. The existing `lhs :=> build` form and `RewriteFun`
type remain available: they receive a map of variable names to `MatchInfo` and
return an optional expression (`Fix l`).

The pattern builder uses the existing `Pattern`, substitution, analysis, and
insertion machinery:

```haskell
type PatternRewriteFun a l =
    VarsState -> Subst -> EGraph a l -> Maybe (Pattern l)

rewriteLhs :: Rewrite a l -> Pattern l
rewriteRhs :: Traversable l
           => Rewrite a l -> VarsState -> Subst -> EGraph a l
           -> Maybe (Pattern l)
```

`ComputePattern` takes a LHS pattern and a `PatternRewriteFun`. Its builder can
inspect analysis or nodes using the same inputs as a `:|` condition. A capture
named `"x"` is found with
`findSubst (findVarName vars "x") subst`; `_class` resolves its representative
in the supplied graph. Returning `"x"` in the RHS reuses that capture. Returning
`pat (Add "x" (pat (Number n)))` builds an addition with a computed constant and
the same captured child. No extraction, copied expression, mutable builder,
new pattern language, or additional dependency is needed. The `rewriteRhs`
helper also handles static rewrites and the existing expression builders,
converting their expressions into patterns without variables.

## Contract

* `Nothing` means that this match is not applicable. No nodes or merges result
  from that refusal. A builder has no graph state to mutate.
* A successful RHS must be semantically equivalent to the LHS under the facts
  proved by the builder and its conditions. The graph cannot retract the
  equality if later information invalidates an assumption. Builders must not
  guess from unknown facts or choose arbitrarily between conflicting facts.
* Every RHS variable must be bound by the LHS. `rewriteRhs` validates the whole
  pattern before insertion. An unbound name is a malformed-rule error, not a
  refusal. This is not a general exception-handling or rollback facility for
  bugs in language analyses.
* The standard runner filters matches by their `:|` conditions using the
  matching snapshot before updating scheduler statistics. Before application,
  `rewriteRhs` rechecks conditions outside-in against the current graph and
  short-circuits before RHS construction. Static and both forms of computed
  RHS use the same insertion and root merge.
* Builders see the standard runner's current graph immediately before an
  application. Earlier applications in the round may have merged captures.
  Resolve IDs relative to that graph; parent analysis and congruence may still
  await rebuilding. Rules needing complete facts can refuse and be retried as
  subsequent rounds run after rebuild.
* A builder should return the same pattern for unchanged semantic evidence.
  Hash-consing and merging then make repeated application idempotent. Fresh
  payloads or endlessly growing equivalent patterns can consume the runner's
  30-round limit, just like expansive static rules.
* The runner rebuilds after each round. Callers that need fully repaired facts
  for the first round should rebuild pending changes before invoking it.
  It checks class nodes and analysis as well as memo/class counts before stopping.
  Refusals are not cached. Scheduling counts matches that pass their `:|`
  conditions, including those whose builder subsequently refuses. The existing
  backoff policy is preserved: at a fixed point, bans are cleared for another
  round only when no rules matched. If other rules still match without making
  progress, the runner can stop while a rule remains banned. Custom runners own
  their scheduling and retry policy.

## Combining constants

[ComputedRewrites.hs](../test/ComputedRewrites.hs) includes a small integer
language and a `combineConstants` rule for `(x + a) + b`. The builder reads
known values for `a` and `b` from analysis, computes their sum, and returns
`x + Number (a + b)`. The expression captured by `x` is reused unchanged, even
when its value is unknown. For example, `(x + 2) + 3` becomes `x + 5`.
If either constant is unknown, the builder returns `Nothing`.

This example demonstrates computed construction that preserves a capture.
The existing `Analysis.modifyA` hook remains suitable for materializing
constants throughout an e-graph.

## Custom runners

Use `rewriteLhs` for matching and `rewriteRhs` to evaluate a match against the
chosen inspection graph. For `Just rhs`, resolve captures against the current
graph, insert the pattern, and merge its result with the matched root according
to the runner's own policy. Resolve inspection IDs in the graph being inspected:
a representative introduced by later mutations may not exist in an earlier
snapshot. Custom runners remain responsible for rebuilding, scheduling, and
retrying matches when relevant facts change.
