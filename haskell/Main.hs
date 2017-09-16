type Name = String

data Type
  = TCon Name
  | TArr Type Type
  deriving (Eq)

instance Show Type where
  show t =
    case t of
      TCon name -> name
      TArr l r -> "(" ++ (show l) ++ " -> " ++ (show r) ++ ")"

tarr :: [Type] -> Type
tarr = foldr1 TArr

tbool = TCon "Bool"
tint = TCon "Int"

data Expr
  = EVar Name
  | EAbs Name Type Expr
  | EApp Expr Expr
  deriving (Eq)

instance Show Expr where
  show e =
    case e of
      EVar name -> name
      EAbs a t b -> "(\\" ++ a ++ " : " ++ (show t) ++ " -> " ++ (show b) ++ ")"
      EApp l r -> "(" ++ (show l) ++ " " ++ (show r) ++ ")"

eapp :: [Expr] -> Expr
eapp = foldl1 EApp

eabs :: [(Name, Type)] -> Expr -> Expr
eabs l b = foldr (\(n, t) e -> EAbs n t e) b l

data Elem
  = CVar Name Type
  deriving (Eq, Show)

data Context = Context [Elem]
  deriving (Eq, Show)

extend (Context c) e = Context (e : c)

findVar :: Context -> Name -> Either String Type
findVar (Context c) n =
  case c of
    [] -> Left $ "Var not found: " ++ n
    h : rest ->
      case h of
        CVar name t -> if name == n then Right t else findVar (Context rest) n

typecheck :: Context -> Expr -> Either String Type
typecheck c e =
  case e of
    EVar n -> findVar c n
    EAbs a t b -> do
      tb <- typecheck (extend c (CVar a t)) b
      return $ TArr t tb
    EApp l r -> do
      tl <- typecheck c l
      tr <- typecheck c r
      case tl of
        TArr ta tb ->
          if ta == tr then
            Right tb
          else
            Left "type mismatch" 
        _ -> Left "Invalid application"

main = putStr $ show $ typecheck (Context []) $ eabs [("x", tbool), ("y", tint)] (EVar "x")
