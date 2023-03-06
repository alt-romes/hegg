-- Set these
:set -package hegg -package deriving-compat -XDeriveTraversable -XTemplateHaskell -XTypeApplications

-- Import these
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

import Data.Equality.Graph
import Data.Equality.Utils
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Language
import Data.Equality.Analysis
import Data.Equality.Graph.Lens ((^.), _data)


-- Define this
:{
data SymExpr a = Const Double
               | Symbol String
               | a :+: a
               | a :*: a
               | a :/: a
               deriving (Functor, Foldable, Traversable)
infix 6 :+:
infix 7 :*:, :/:

deriveEq1   ''SymExpr
deriveOrd1  ''SymExpr
deriveShow1 ''SymExpr

instance Language SymExpr
:}

:{
x :: ENode SymExpr
x = Node $ Symbol "x"

y :: ENode SymExpr
y = Node $ Symbol "y"

c :: ENode SymExpr
c = Node $ Const 2

plus :: ClassId -> ClassId -> ENode SymExpr
plus = fmap Node . (:+:)
:}

(x', eg)  = add @() x emptyEGraph
(y', eg') = add @() y eg
(c', eg)  = add @() c eg'
(f1, eg') = add @() (plus x' c') eg
(f2, eg)  = add @() (plus y' c') eg'
(merge_id,eg') = merge x' y' eg
find x' eg'
find y' eg'
find f1 eg'
find f2 eg'
-- Congruence wasn't mantained!!
-- E-graphs typically call rebuild after every add and merge
-- But we defer it to the call of rebuild which can be whenever we'd like
-- (which is great for eqsat)
eg = rebuild eg'
find f1 eg
find f2 eg

e1 :: Fix SymExpr
e1 = Fix (Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: Fix (Const 2)) -- (x*2)/2

e2 :: Fix SymExpr
e2 = Fix (Fix (Fix (Symbol "x") :*: Fix (Const 2)) :/: Fix (Const 2)) -- (x*2)/2
:}

