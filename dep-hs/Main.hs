{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (fromRight)
import Data.List (find)
import GHC.Exts(IsString(..))
import Debug.Trace (trace)
import Control.Monad (unless)

data Term
  = Var String
  | Uni Int
  | App Term Term
  | Abs String Term Term
  | Pi String Term Term
  | Label
  | LabelLit String
  | Row
  | EmptyRow
  | ExtendRow Term Term Term
  deriving (Eq)

instance Show Term where
  show (Var s) = s
  show (Uni n) = "*" ++ (show n)
  show (App l r) = "(" ++ (show l) ++ " " ++ (show r) ++ ")"
  show (Abs s t b) = "(\\(" ++ s ++ ":" ++ (show t) ++ ")." ++ (show b) ++ ")"
  show (Pi s t b) = "((" ++ s ++ ":" ++ (show t) ++ ") -> " ++ (show b) ++ ")"
  show Label = "Label"
  show (LabelLit s) = "'" ++ s
  show Row = "Row"
  show EmptyRow = "<>"
  show (ExtendRow l t r) = "<" ++ (show l) ++ " : " ++ (show t) ++ " | " ++ (show r) ++ ">"

instance IsString Term where
  fromString = Var

data ITerm
  = IFree String
  | IBound Int
  | IUni Int
  | IApp ITerm ITerm
  | IAbs String ITerm ITerm
  | IPi String ITerm ITerm
  | ILabel
  | ILabelLit String
  | IRow
  | IEmptyRow
  | IExtendRow ITerm ITerm ITerm
  deriving (Eq)

instance Show ITerm where
  show (IFree s) = s
  show (IBound n) = "'" ++ (show n)
  show (IUni n) = "*" ++ (show n)
  show (IApp l r) = "(" ++ (show l) ++ " " ++ (show r) ++ ")"
  show (IAbs _ t b) = "(\\" ++ (show t) ++ "." ++ (show b) ++ ")"
  show (IPi _ t b) = "(" ++ (show t) ++ " -> " ++ (show b) ++ ")"
  show ILabel = "Label"
  show (ILabelLit s) = "'" ++ s
  show IRow = "Row"
  show IEmptyRow = "<>"
  show (IExtendRow l t r) = "<" ++ (show l) ++ " : " ++ (show t) ++ " | " ++ (show r) ++ ">"

data ContextElem
  = CVar String ITerm
  | CDef String ITerm
  deriving (Eq)

instance Show ContextElem where
  show (CVar n t) = n ++ " : " ++ (show t)
  show (CDef n t) = n ++ " = " ++ (show t)

cvar :: String -> Term -> ContextElem
cvar n = CVar n . toInternal 

cdef :: String -> Term -> ContextElem
cdef n = CDef n . toInternal 

data Context = Context [ContextElem]

instance Show Context where
  show (Context ctx) = "Context" ++ (show $ reverse ctx)

context :: [ContextElem] -> Context
context = Context . reverse

extend :: ContextElem -> Context -> Context
extend e (Context c) = Context $ e : c

addDef :: (String, Term) -> Context -> Either String Context
addDef (n, t) ctx =
  do (ctx, term, typ) <- eval ctx t
     return $ extend (cdef n term) $ extend (cvar n typ) ctx

findVar :: String -> Context -> Either String ITerm
findVar n ctx@(Context c) =
  case (fmap (\(CVar n t) -> t) $ find (\e ->
    case e of
      CVar m t -> m == n
      _ -> False) c) of
    Nothing -> Left $ "Cannot find var " ++ n ++ " in " ++ (show ctx)
    Just t -> Right t

findDef :: String -> Context -> Either String ITerm
findDef n ctx@(Context c) =
  case (fmap (\(CDef n t) -> t) $ find (\e ->
    case e of
      CDef m t -> m == n
      _ -> False) c) of
    Nothing -> Left $ "Cannot find def " ++ n ++ " in " ++ (show ctx)
    Just t -> Right t

toInternal' :: Int -> Map.Map String Int -> Term -> ITerm
toInternal' lv m (Var s) = maybe (IFree s) (\n -> IBound (lv - n - 1)) $ Map.lookup s m
toInternal' lv m (Uni n) = IUni n
toInternal' lv m (LabelLit s) = ILabelLit s
toInternal' lv m Label = ILabel
toInternal' lv m Row = IRow
toInternal' lv m EmptyRow = IEmptyRow
toInternal' lv m (ExtendRow l t r) = IExtendRow (toInternal' lv m l) (toInternal' lv m t) (toInternal' lv m r)
toInternal' lv m (App l r) = IApp (toInternal' lv m l) (toInternal' lv m r)
toInternal' lv m (Abs s t b) =
  let n = Map.insert s lv m in
  IAbs s (toInternal' (lv + 1) n t) (toInternal' (lv + 1) n b)
toInternal' lv m (Pi s t b) =
  let n = Map.insert s lv m in
  IPi s (toInternal' (lv + 1) n t) (toInternal' (lv + 1) n b)

toInternal :: Term -> ITerm
toInternal = toInternal' 0 Map.empty

open :: ITerm -> ITerm -> ITerm
open u t = go 0 u t where
  go k u t@(IBound n) = if n == k then u else t
  go k u (IApp l r) = IApp (go k u l) (go k u r)
  go k u (IExtendRow l t r) = IExtendRow (go k u l) (go k u t) (go k u r)
  go k u (IAbs s t b) = IAbs s (go (k + 1) u t) (go (k + 1) u b)
  go k u (IPi s t b) = IPi s (go (k + 1) u t) (go (k + 1) u b)
  go _ _ t = t

openVar :: String -> ITerm -> ITerm
openVar = open . IFree

close :: String -> ITerm -> ITerm
close x t = go 0 x t where
  go k u t@(IFree y) = if y == x then (IBound k) else t
  go k u (IApp l r) = IApp (go k u l) (go k u r)
  go k u (IExtendRow l t r) = IExtendRow (go k u l) (go k u t) (go k u r)
  go k u (IAbs s t b) = IAbs s (go (k + 1) u t) (go (k + 1) u b)
  go k u (IPi s t b) = IPi s (go (k + 1) u t) (go (k + 1) u b)
  go _ _ t = t

subst :: String -> ITerm -> ITerm -> ITerm
subst x u = open u . close x

free :: ITerm -> Set.Set String
free (IFree x) = Set.singleton x
free (IApp l r) = Set.union (free l) (free r)
free (IExtendRow l t r) = Set.union (free l) $ Set.union (free t) (free r)
free (IAbs _ t b) = Set.union (free t) (free b)
free (IPi _ t b) = Set.union (free t) (free b)
free _ = Set.empty

freshIn :: String -> ITerm -> Bool
freshIn x t = Set.notMember x (free t)

isLocallyClosed :: ITerm -> Bool
isLocallyClosed t = go 0 t where
  go k (IBound n) = n >= 0 && n < k
  go k (IApp l r) = go k l && go k r
  go k (IExtendRow l t r) = go k l && go k t && go k r
  go k (IAbs _ t b) = go (k + 1) t && go (k + 1) b
  go k (IPi _ t b) = go (k + 1) t && go (k + 1) b
  go _ _ = True

isClosed :: ITerm -> Bool
isClosed = Set.null . free

toExternal :: ITerm -> Term
toExternal (IFree x) = Var x
toExternal (IBound n) = Var (show n)
toExternal (IUni n) = Uni n
toExternal (ILabelLit s) = LabelLit s
toExternal ILabel = Label
toExternal IRow = Row
toExternal IEmptyRow = EmptyRow
toExternal (IExtendRow l t r) = ExtendRow (toExternal l) (toExternal t) (toExternal r)
toExternal (IApp l r) = App (toExternal l) (toExternal r)
toExternal (IAbs n t b) = Abs n (toExternal $ openVar n t) (toExternal $ openVar n b)
toExternal (IPi n t b) = Pi n (toExternal $ openVar n t) (toExternal $ openVar n b)

fresh :: String -> ITerm -> String
fresh x t = if freshIn x t then x else fresh (x ++ "'") t

normalize :: Context -> ITerm -> ITerm
normalize ctx t@(IFree n) = fromRight t $ findDef n ctx
normalize ctx (IApp l r) =
  let
    l' = normalize ctx l
    r' = normalize ctx r
  in
    case l' of
      IAbs n t b -> normalize ctx $ open r' b
      _ -> IApp l' r'
normalize ctx (IExtendRow l t r) = IExtendRow (normalize ctx l) (normalize ctx t) (normalize ctx r)
normalize ctx (IAbs n t b) =
  case b of
    IApp l (IBound 0) -> normalize ctx l
    _ ->
      let x = fresh (fresh n t) b in
      IAbs n (close x $ normalize ctx $ openVar x t) (close x $ normalize ctx $ openVar x b)
normalize ctx (IPi n t b) =
  case b of
    IApp l (IBound 0) -> normalize ctx l
    _ ->
      let x = fresh (fresh n t) b in
      IPi n (close x $ normalize ctx $ openVar x t) (close x $ normalize ctx $ openVar x b)
normalize _ t = t

equivalent :: Context -> ITerm -> ITerm -> Bool
equivalent ctx a b = equivalent' (normalize ctx a) (normalize ctx b) where
  equivalent' (IFree n) (IFree m) = n == m
  equivalent' (IBound n) (IBound m) = n == m
  equivalent' (IUni n) (IUni m) = n == m
  equivalent' (IApp l r) (IApp a b) = equivalent' l a && equivalent' r b
  equivalent' (IAbs _ l r) (IAbs _ a b) = equivalent' l a && equivalent' r b
  equivalent' (IPi _ l r) (IPi _ a b) = equivalent' l a && equivalent' r b
  equivalent' ILabel ILabel = True
  equivalent' (ILabelLit x) (ILabelLit y) = x == y
  equivalent' IRow IRow = True
  equivalent' IEmptyRow IEmptyRow = True
  equivalent' (IExtendRow a b c) (IExtendRow x y z) = equivalent' a x && equivalent' b y && equivalent' c z
  equivalent' _ _ = False

inferUniverse :: Context -> ITerm -> Either String (Context, Int)
inferUniverse ctx t = do
  (ctx, u) <- infer ctx t
  case normalize ctx u of
    IUni n -> return (ctx, n)
    ILabel -> return (ctx, 1)
    IRow -> return (ctx, 1)
    t -> Left $ "Unexpected type in inferUniverse " ++ (show t) ++ " in " ++ (show ctx)

infer :: Context -> ITerm -> Either String (Context, ITerm)
infer ctx t =
  if not $ isLocallyClosed t then
    Left $ "Term is not locally closed " ++ (show t) ++ " in " ++ (show ctx)
  else
    case t of
      IFree n ->
        do t <- findVar n ctx
           return (ctx, t)
      IBound n -> Left $ "Bound variable found in infer " ++ (show n) ++ " in " ++ (show ctx)
      ILabel -> return (ctx, IUni 1)
      ILabelLit x -> return (ctx, ILabel)
      IRow -> return (ctx, IUni 1)
      IEmptyRow -> return (ctx, IRow)
      IUni n -> return (ctx, IUni $ n + 1)
      IPi n t b ->
        do let x = fresh (fresh n t) b
           (ctx, ut) <- inferUniverse ctx t
           (_, ub) <- inferUniverse (extend (CVar x t) ctx) (openVar x b)
           return (ctx, IUni $ max ut ub)
      IAbs n t b ->
        do let x = fresh (fresh n t) b
           (ctx, _) <- inferUniverse ctx t
           (_, typ) <- infer (extend (CVar x t) ctx) (openVar x b)
           return (ctx, IPi n t $ close x typ)
      IApp l r ->
        do (ctx, tl) <- infer ctx l
           case normalize ctx tl of
             IPi n typ b ->
               do (ctx, tr) <- infer ctx r
                  if equivalent ctx typ tr then
                    return (ctx, open r b)
                  else
                    Left $ "Invalid application, right side has invalid type " ++ (show tl) ++ " and " ++ (show tr) ++ " in " ++ (show t) ++ " in " ++ (show ctx)
             tl -> Left $ "Invalid application, left side is not a pi type " ++ (show tl) ++ " in " ++ (show t) ++ " in " ++ (show ctx)
      IExtendRow l typ r ->
        do (ctx, tl) <- infer ctx l
           unless (equivalent ctx tl ILabel) (Left $ "Label in row extension is not of type Label " ++ (show tl) ++ " in " ++ (show t) ++ " in " ++ (show ctx))
           (ctx, tt) <- infer ctx typ
           unless (equivalent ctx tt (IUni 0)) (Left $ "Type in row extension is not of type * " ++ (show tt) ++ " in " ++ (show t) ++ " in " ++ (show ctx))
           (ctx, tr) <- infer ctx r
           unless (equivalent ctx tr IRow) (Left $ "Rest in row extension is not of type Row * " ++ (show tr) ++ " in " ++ (show t) ++ " in " ++ (show ctx))
           return (ctx, IRow)

eval :: Context -> Term -> Either String (Context, Term, Term)
eval ctx term =
  do let term' = toInternal term
     (ctx, typ) <- infer ctx term'
     return (ctx, toExternal $ normalize ctx term', toExternal $ normalize ctx typ)

star = Uni 0
($$) = App
infixl 0 $$
(~~>) = uncurry Pi
infixr 1 ~~>
(-->) = Pi "_"
infixr 1 -->
(==>) = uncurry Abs
infixr 1 ==>

ctx =
  context [
    cvar "Nat" star,
    cvar "z" "Nat",
    cvar "s" ("Nat" --> "Nat"),

    cvar "inc" ("Nat" --> "Nat"),
    cdef "inc" "s",

    cvar "id" (("t", star) ~~> ("x", "t") ~~> "t"),
    cdef "id" (("t", star) ==> ("x", "t") ==> "x")
  ]
term = ("l", Label) ==> ExtendRow "l" "Nat" EmptyRow $$ LabelLit "x"
main = putStr $ case eval ctx term of
  Left err -> err
  Right (ctx, t, typ) -> (show t) ++ " : " ++ (show typ) ++ "\n" ++ (show ctx)
