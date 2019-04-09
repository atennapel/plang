import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List

type Name = String

data Term
  = Var Name
  | Abs [Name] Term
  | App Term [Term]
  | Atom Name
  | Pair Val Val
  | PairC Term Term

instance Show Term where
  show (Var x) = x
  show (Abs x b) = "(\\" ++ (intercalate " " x) ++ " -> " ++ (show b) ++ ")"
  show (App l r) = "(" ++ (show l) ++ " " ++ (intercalate " " $ map show r) ++ ")"
  show (Atom x) = x
  show (Pair a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
  show (PairC a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"

free :: Term -> Set Name
free (Var x) = Set.singleton x
free (Abs x b) = Set.difference (free b) (Set.fromList x)
free (App l r) = Set.union (free l) (Set.unions $ map free r)
free (PairC l r) = Set.union (free l) (free r)
free _ = Set.empty

data Val
  = VClos [Name] Term EnvStack
  | VAtom Name
  | VPair Val Val
  
instance Show Val where
  show (VClos xs b e) = "Clos(\\" ++ (intercalate " " xs) ++ " -> " ++ (show b) ++ ", " ++ (show e) ++ ")"
  show (VAtom x) = x
  show (VPair l r) = "(" ++ (show l) ++ ", " ++ (show r) ++ ")"

type Env = Map Name Val
type EnvStack = [Env]

data Frame
  = FArg [Term] EnvStack
  | FFun [Name] Term EnvStack [Term] [Val] EnvStack
  | FPairFst Term EnvStack
  | FPairSnd Val
  deriving (Show)

data State = State Term EnvStack [Frame]
  deriving (Show)

lookupEnv :: Name -> EnvStack -> Maybe Val
lookupEnv _ [] = Nothing
lookupEnv x (h : t) = maybe (lookupEnv x t) return (Map.lookup x h)

filterEnv :: Set Name -> EnvStack -> EnvStack
filterEnv _ [] = []
filterEnv fr (e : t) =
  let e' = Map.restrictKeys e fr in
  let rest = filterEnv fr t in
  if Map.null e' then rest else e' : rest

makeClos :: [Name] -> Term -> EnvStack -> Val
makeClos x b e =
  let fr = free b in
  VClos x b (filterEnv fr e)

stepApp :: Term -> Val -> [Name] -> Term -> EnvStack -> [Term] -> [Val] -> EnvStack -> [Frame] -> Maybe State
stepApp t v xs b e ts' vs tse fs =
  let lxs = length xs in
  let lvs = length vs in
  if lvs < lxs then -- enough arguments yet not
    case ts' of
      [] -> -- we have to apply
        if lvs + 1 == lxs then -- full application
          Just $ State b ((Map.fromList $ zip xs (reverse (v : vs))) : e) fs 
        else -- partial application
          Just $ State (Abs (drop (lvs + 1) xs) b) ((Map.fromList $ zip xs (reverse (v : vs))) : e) fs
      a : as -> -- evaluate next argument
        Just $ State a tse (FFun xs b e as (v : vs) tse : fs)
  else -- over application
    Just $ State b ((Map.fromList $ zip xs (reverse vs)) : e) (FArg (t : ts') tse : fs)


step :: State -> Maybe State
step (State t e fs) =
  case t of
    Var i ->
      case lookupEnv i e of
        Nothing -> Nothing
        Just (VClos x b e') -> Just (State (Abs x b) e' fs)
        Just (VAtom x) -> Just (State (Atom x) e fs)
        Just (VPair l r) -> Just (State (Pair l r) e fs)
    App l r -> Just $ State l e (FArg r e : fs)
    PairC l r -> Just $ State l e (FPairFst r e : fs)
    Abs x b ->
      case fs of
        [] -> Nothing
        FArg [] _ : fs' ->
          case x of
            [] -> Just $ State b e fs'
            _ -> Just $ State t e fs'
        FArg (a : as) e' : fs' -> Just $ State a e' (FFun x b e as [] e' : fs')
        FFun x' b' e' ts' vs tse : fs' -> stepApp t (makeClos x b e) x' b' e' ts' vs tse fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (makeClos x b e) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (makeClos x b e)) e fs'
    Atom x ->
      case fs of
        [] -> Nothing
        FArg _ _ : _ -> Nothing
        FFun x' b' e' ts' vs tse : fs' -> stepApp t (VAtom x) x' b' e' ts' vs tse fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (VAtom x) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (VAtom x)) e fs'
    Pair l r ->
      case fs of
        [] -> Nothing
        FArg _ _ : _ -> Nothing
        FFun x' b' e' ts' vs tse : fs' -> stepApp t (VPair l r) x' b' e' ts' vs tse fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (VPair l r) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (VPair l r)) e fs'

start :: Term -> State
start t = State t [] []

steps :: State -> State
steps s =
  case {-trace (show s) -} step s of
    Nothing -> s
    Just s' -> steps s'

tid :: Term
tid = Abs ["x"] $ Var "x"

tconst :: Term
tconst = Abs ["x", "y"] $ Var "x"

tz :: Term
tz = Abs ["f", "x"] $ Var "x"

ts :: Term
ts = Abs ["n", "f", "x"] $ App (Var "f") [App (Var "n") [Var "f", Var "x"]]

pow :: Term
pow = Abs ["n", "m"] $ App (Var "m") [Var "n"]

tshowNat :: Term -> Term
tshowNat t = App t [Abs ["x"] $ PairC (Atom "S") (Var "x"), Atom "Z"]

n0 :: Term
n0 = tz

n1 :: Term
n1 = App ts [tz]

n2 :: Term
n2 = App ts [App ts [tz]]

n3 :: Term
n3 = App ts [App ts [App ts [tz]]]

term :: Term
term = App tconst [App (Abs ["f", "x", "y", "z"] $ Var "f") [Atom "f", Atom "x", Atom "y", Atom "z"]]

main :: IO ()
main =
  let (State t _ _) = steps $ start term in
  putStrLn $ show t
