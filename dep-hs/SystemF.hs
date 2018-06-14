{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (fromRight)
import Data.List (find)
import GHC.Exts(IsString(..))
import Debug.Trace (trace)
import Control.Monad (unless)

type Name = String

-- External AST
data Kind
  = KType
  | KArr Kind Kind
  deriving (Eq)

instance Show Kind where
  show KType = "Type"
  show (KArr l r) = "(" ++ (show l) ++ " -> " ++ (show r) ++ ")"

data Type
  = TVar Name
  | TArr Type Type
  | TForall Name Kind Type

instance Show Type where
  show (TVar n) = n
  show (TArr l r) = "(" ++ (show l) ++ " -> " ++ (show r) ++ ")"
  show (TForall n k t) = "(forall (" ++ n ++ " : " ++ (show k) ++ ") . " ++ (show t) ++ ")"

data Expr
  = EVar Name
  | EAbs Name Type Expr
  | EApp Expr Expr
  | ETAbs Name Kind Expr
  | ETApp Expr Type

instance Show Expr where
  show (EVar n) = n
  show (EAbs n t e) = "(\\(" ++ n ++ " : " ++ (show t) ++ ") . " ++ (show e) ++ ")"
  show (EApp l r) = "(" ++ (show l) ++ " " ++ (show r) ++ ")"
  show (ETAbs n k e) = "(/(" ++ n ++ " : " ++ (show k) ++ ") . " ++ (show e) ++ ")"
  show (ETApp l r) = "(" ++ (show l) ++ " [" ++ (show r) ++ "])"

-- Internal AST
data IType
  = ITFree Name
  | ITBound Int
  | ITArr IType IType
  | ITForall Name Kind IType

instance Show IType where
  show (ITFree n) = n
  show (ITBound n) = "'" ++ (show n)
  show (ITArr l r) = "(" ++ (show l) ++ " -> " ++ (show r) ++ ")"
  show (ITForall n k t) = "(forall " ++ (show k) ++ " . " ++ (show t) ++ ")"

data IExpr
  = IEFree Name
  | IEBound Int
  | IEAbs Name IType IExpr
  | IEApp IExpr IExpr
  | IETAbs Name Kind IExpr
  | IETApp IExpr IType

instance Show IExpr where
  show (IEFree n) = n
  show (IEBound n) = "'" ++ (show n)
  show (IEAbs n t e) = "(\\" ++ (show t) ++ " . " ++ (show e) ++ ")"
  show (IEApp l r) = "(" ++ (show l) ++ " " ++ (show r) ++ ")"
  show (IETAbs n k e) = "(/" ++ (show k) ++ " . " ++ (show e) ++ ")"
  show (IETApp l r) = "(" ++ (show l) ++ " [" ++ (show r) ++ "])"

-- Locally Nameless
open :: ITerm -> ITerm -> ITerm
open u t = go 0 u t where
  go k u t@(IBound n) = if n == k then u else t
  go k u (IApp l r) = IApp (go k u l) (go k u r)
  go k u (IExtendRow l t r) = IExtendRow (go k u l) (go k u t) (go k u r)
  go k u (IAbs s t b) = IAbs s (go (k + 1) u t) (go (k + 1) u b)
  go k u (IPi s t b) = IPi s (go (k + 1) u t) (go (k + 1) u b)
  go _ _ t = t

close :: String -> ITerm -> ITerm
close x t = go 0 x t where
  go k u t@(IFree y) = if y == x then (IBound k) else t
  go k u (IApp l r) = IApp (go k u l) (go k u r)
  go k u (IExtendRow l t r) = IExtendRow (go k u l) (go k u t) (go k u r)
  go k u (IAbs s t b) = IAbs s (go (k + 1) u t) (go (k + 1) u b)
  go k u (IPi s t b) = IPi s (go (k + 1) u t) (go (k + 1) u b)
  go _ _ t = t

free :: ITerm -> Set.Set String
free (IFree x) = Set.singleton x
free (IApp l r) = Set.union (free l) (free r)
free (IExtendRow l t r) = Set.union (free l) $ Set.union (free t) (free r)
free (IAbs _ t b) = Set.union (free t) (free b)
free (IPi _ t b) = Set.union (free t) (free b)
free _ = Set.empty

isLocallyClosed :: IExpr -> Bool
isLocallyClosed t = go 0 t where
  go k (IEBound n) = n >= 0 && n < k
  go k (IEApp l r) = go k l && go k r
  go k (IEAbs _ t b) = typeIsLocallyClosed t && go (k + 1) b
  go k (IETAbs _ k b) = go k b
  go k (IETApp l r) = go k l && typeIsLocallyClosed r
  go _ _ = True

openVar :: String -> IExpr -> IExpr
openVar = open . IEFree

subst :: String -> IExpr -> IExpr -> IExpr
subst x u = open u . close x

freshIn :: String -> IExpr -> Bool
freshIn x t = Set.notMember x (free t)

isClosed :: IExpr -> Bool
isClosed = Set.null . free

-- Conversion
toInternalType :: Type -> IType
toInternalType = toInternalType' 0 Map.empty

toInternalType' :: Int -> Map.Map String Int -> Type -> IType
toInternalType' lv m (TVar s) = maybe (ITFree s) (\n -> ITBound (lv - n - 1)) $ Map.lookup s m
toInternalType' lv m (TArr l r) = ITArr (toInternalType' lv m l) (toInternalType' lv m r)
toInternalType' lv m (TForall s k t) =
  let n = Map.insert s lv m in
  ITForall s k (toInternalType' (lv + 1) n t)

toInternal :: Expr -> IExpr
toInternal = toInternal' 0 Map.empty

toInternal' :: Int -> Map.Map String Int -> Expr -> IExpr
toInternal' lv m (EVar s) = maybe (IEFree s) (\n -> IEBound (lv - n - 1)) $ Map.lookup s m
toInternal' lv m (EAbs s t e) =
  let n = Map.insert s lv m in
  IEAbs s (toInternalType t) (toInternal' (lv + 1) n e)
toInternal' lv m (EApp l r) = IEApp (toInternal' lv m l) (toInternal' lv m r)
toInternal' lv m (ETAbs s k e) = IETAbs s k (toInternal' lv m e)
toInternal' lv m (ETApp l r) = IETApp (toInternal' lv m l) (toInternalType r)

toExternalType :: IType -> Type
toExternalType (ITFree s) = TVar s
toExternalType (ITBound _) = error "Unexpected bound type variable in toExternalType"
toExternalType (ITArr l r) = TArr (toExternalType l) (toExternalType r)
toExternalType (ITForall n k t) = TForall n k (toExternalType $ typeOpenVar n t)

toExternal :: IExpr -> Expr
toExternal (IEFree s) = EVar s
toExternal (IEBound _) = error "Unexpected bound variable in toExternal"
toExternal (IEAbs n t e) = EAbs n (toExternalType t) (toExternal $ openVar n e)
toExternal (IEApp l r) = EApp (toExternal l) (toExternal r)
toExternal (IETAbs n k e) = ETAbs n k (toExternal e)

-- Testing
main = putStr $ show $ toInternal $ EAbs "x" (TVar "Int") (EVar "x")
