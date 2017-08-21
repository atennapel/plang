{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}

type Name = String

data Id = Id Name Int

instance Show Id where
  show (Id name n) = name ++ (if n == 0 then "" else show n)

data TypeKind = Mono | Poly

data Type :: TypeKind -> * where
  TUnit :: Type k
  TVar :: Id -> Type k
  TExists :: Id -> Type k
  TForall :: Id -> Type Poly -> Type Poly
  TArr :: Type k -> Type k -> Type k
deriving instance Show (Type k)

tunit = TUnit
tvar = TVar
texists = TExists

(-->) = TArr
infixr 1 -->

tforalls = flip (foldr TForall)

x = Id "x" 0
y = Id "y" 0

main = putStr (show $ tforalls [x, y] (tvar x --> tvar x))
