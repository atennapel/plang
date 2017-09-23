{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (fromRight)
import Data.List (find)
import GHC.Exts(IsString(..))
import Debug.Trace (trace)
import Control.Monad (unless)

-- External AST

type Name = String

data Kind
  = KType
  | KRow
  | KLabel
  | KArr Kind Kind
  deriving (Eq)

instance Show Kind where
  show KType = "Type"
  show KRow = "Row"
  show KLabel = "Label"
  show (KArr l r) = "(" ++ (show l) ++ " -> " ++ (show r) ++ ")"

data Type
  = TVar Name
  | TCon Name
  | TArr Type Type
  | TForall Name Kind Type

instance Show Type where
  show (TVar n) = n
  show (TCon n) = n
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

main = putStr $ show $ ETApp (ETAbs "t" KType $ EAbs "x" (TVar "t") (EVar "t")) (TCon "Int")
