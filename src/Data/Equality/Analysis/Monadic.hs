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

import Data.Equality.Graph.Internal (EGraph)
import Data.Equality.Graph.Classes

-- | An e-class analysis with domain @domain@ defined for a language @l@, whose operations are only well-defined within some effectful context.
--
-- The @domain@ is the type of the domain of the e-class analysis, that is, the
-- type of the data stored in an e-class according to this e-class analysis
class (Monad m, Eq domain) => AnalysisM m domain (l :: Type -> Type) where

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
    modifyA :: ClassId
            -- ^ Id of class @c@ whose new data @d_c@ triggered the modify call
            -> EGraph domain l
            -- ^ E-graph where class @c@ being modified exists
            -> m (EGraph domain l)
            -- ^ E-graph resulting from the modification
    modifyA _ = pure
    {-# INLINE modifyA #-}


-- | The simplest analysis that defines the domain to be () and does nothing otherwise
instance Monad m => AnalysisM m () l where
  makeA _ = pure ()
  joinA _ _ = pure ()

