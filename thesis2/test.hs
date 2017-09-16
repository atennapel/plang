module Lambda ( Term
, open
, varOpen
, varClose
, betaReduce
, subst
, freeVars
, freshIn
, isLocallyClosed
, isClosed
) where

import qualified Data.Set as S
import Numeric.Natural -- from the nats package <https://hackage.haskell.org/package/nats>

-- ----------------------------------------------------
-- Untyped λ-calculus terms using the locally nameless
-- representation due to Chargueraud:
-- <http://www.chargueraud.org/research/2009/ln/main.pdf>
-- ----------------------------------------------------
type Id = String
data Term = BVar Natural
| FVar Id
| Abs  Term
| App  Term Term
deriving Show

-- | Open a term by substituting another term for the top bound variable
open u t = go 0 u t
where
go k u t@(BVar m)   = if m == k
                    then u
                    else t
go _ _ t@(FVar _)   = t
go k u  (Abs t')    = Abs (go (succ k) u t')
go k u  (App t1 t2) = App (go k u t1) (go k u t2)

-- | Open a term with respect to the top bound variable
varOpen x = open (FVar x)

-- | Close a term with respect to a free variable
varClose x t = go 0 x t
where
go _ _ t@(BVar _)    = t
go k x t@(FVar y)    = if y == x
                     then BVar k
                     else t
go k x   (Abs t')    = Abs (go (succ k) x t')
go k x   (App t1 t2) = App (go k x t1) (go k x t2)

-- | Perform one step of β-reduction
betaReduce (App (Abs t1) t2) = open t2 t1
betaReduce t                 = t

-- | Substitute a term for a free variable
subst x u = open u . varClose x

-- | Set of free variables of a term
freeVars (BVar _)    = S.empty
freeVars (FVar x)    = S.singleton x
freeVars (Abs t')    = freeVars t'
freeVars (App t1 t2) = S.union (freeVars t1) (freeVars t2)

-- | Check whether a variable name is fresh in given term
freshIn x t = x `S.notMember` freeVars t

-- | Check whether a term is locally closed, i.e. all bound variables
-- have their corresponding binding abstractions
isLocallyClosed t = go 0 t
where
go k (BVar m)    = m < k
go _ (FVar _)    = True
go k (Abs t')    = go (succ k) t'
go k (App t1 t2) = go k t1 && go k t2

-- | Check whether a term is closed, i.e. it has no free variables
isClosed = S.null . freeVars


one = Abs (Abs (App (BVar 0) (BVar 1)))
