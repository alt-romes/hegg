{-# LANGUAGE AllowAmbiguousTypes #-} -- joinA
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-|

Like 'Data.Equality.Analysis' but for 'Analysis' that are only well-defined
within an (effectful) context. Mostly used with the monadic operations
'representM', 'addM', 'mergeM', and 'rebuildM'.

This effectful 'Analysis' could almost be trivially defined in terms of the
other, through a "contextful" domain and by means of the '_classes' 'Traversal'.

However, that would require an instance of 'Eq' for the monadic domain, which
is usually unnattainable.

Therefore, we do need this class for monadic 'Analysis'.

-}
module Data.Equality.Analysis.Monadic where


import Data.Kind (Type)

import Data.Equality.Utils
import Data.Equality.Graph.Classes

-- | An e-class analysis with domain @domain@ defined for a language @l@, whose operations are only well-defined within some effectful context.
--
-- The @domain@ is the type of the domain of the e-class analysis, that is, the
-- type of the data stored in an e-class according to this e-class analysis
class (Monad m, Eq domain) => Analysis m domain (l :: Type -> Type) where

    -- | When a new e-node is added into a new, singleton e-class, construct a
    -- new value of the domain to be associated with the new e-class, by
    -- accessing the associated data of the node's children
    --
    -- The argument is the e-node term populated with its children data
    makeA :: l domain -> m domain

    -- | When e-classes c1 c2 are being merged into c, join d_c1 and
    -- d_c2 into a new value d_c to be associated with the new
    -- e-class c
    joinA :: domain -> domain -> m domain

    -- | Optionally modify the e-class c (based on d_c), typically by adding an
    -- e-node to c. Modify should be idempotent if no other changes occur to
    -- the e-class, i.e., modify(modify(c)) = modify(c)
    --
    -- The return value of the modify function is both the modified class and
    -- the expressions (in their fixed-point form) to add to this class. We
    -- can't manually add them because not only would it skip some of the
    -- internal steps of representing + merging, but also because it's
    -- impossible to add any expression with depth > 0 without access to the
    -- e-graph (since we must represent every sub-expression in the e-graph
    -- first).
    --
    -- That's why we must return the modified class and the expressions to add
    -- to this class.
    modifyA :: EClass domain l -> m (EClass domain l, [Fix l])
    modifyA c = pure (c, [])
    {-# INLINE modifyA #-}


-- | The simplest analysis that defines the domain to be () and does nothing otherwise
instance Monad m => Analysis m () l where
  makeA _ = pure ()
  joinA _ _ = pure ()

