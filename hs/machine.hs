import Debug.Trace
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type Name = String

data Term
  = Var Name
  | Abs Name Term
  | App Term Term
  | Atom Name
  | Pair Val Val
  | PairC Term Term

instance Show Term where
  show (Var x) = x
  show (Abs x b) = "(\\" ++ x ++ " -> " ++ (show b) ++ ")"
  show (App l r) = "(" ++ (show l) ++ " " ++ (show r) ++ ")"
  show (Atom x) = x
  show (Pair a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
  show (PairC a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"

free :: Term -> Set Name
free (Var x) = Set.singleton x
free (Abs x b) = Set.delete x (free b)
free (App l r) = Set.union (free l) (free r)
free (PairC l r) = Set.union (free l) (free r)
free _ = Set.empty

data Val
  = VClos Name Term EnvStack
  | VAtom Name
  | VPair Val Val

instance Show Val where
  show (VClos x b e) = "Clos(\\" ++ x ++ " -> " ++ (show b) ++ ", " ++ (show e) ++ ")"
  show (VAtom x) = x
  show (VPair l r) = "(" ++ (show l) ++ ", " ++ (show r) ++ ")"

type Env = Map Name Val
type EnvStack = [Env]

data Frame
  = FArg Term EnvStack
  | FFun Name Term EnvStack
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

makeClos :: Name -> Term -> EnvStack -> Val
makeClos x b e =
  let fr = free b in
  VClos x b (filterEnv fr e)

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
        FArg r e' : fs' -> Just $ State r e' (FFun x b e : fs')
        FFun x' b' e' : fs' -> Just $ State b' (Map.singleton x' (makeClos x b e) : e') fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (makeClos x b e) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (makeClos x b e)) e fs'
    Atom x ->
      case fs of
        [] -> Nothing
        FArg _ _ : _ -> Nothing
        FFun x' b' e' : fs' -> Just $ State b' (Map.singleton x' (VAtom x) : e') fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (VAtom x) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (VAtom x)) e fs'
    Pair l r ->
      case fs of
        [] -> Nothing
        FArg _ _ : _ -> Nothing
        FFun x' b' e' : fs' -> Just $ State b' (Map.singleton x' (VPair l r) : e') fs'
        FPairFst t' e' : fs' -> Just $ State t' e' (FPairSnd (VPair l r) : fs')
        FPairSnd v : fs' -> Just $ State (Pair v (VPair l r)) e fs'

start :: Term -> State
start t = State t [] []

steps :: State -> State
steps s =
  case {-trace (show s) $ -} step s of
    Nothing -> s
    Just s' -> steps s'

tid :: Term
tid = Abs "x" (Var "x")

tconst :: Term
tconst = Abs "x" $ Abs "y" (Var "x")

tz :: Term
tz = Abs "f" $ Abs "x" $ Var "x"

ts :: Term
ts = Abs "n" $ Abs "f" $ Abs "x" $ App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))

tshowNat :: Term -> Term
tshowNat t = App (App t (Abs "x" $ PairC (Atom "S") (Var "x"))) (Atom "Z")

pow :: Term
pow = Abs "n" $ Abs "m" $ App (Var "m") (Var "n")

n0 :: Term
n0 = tz

n2 :: Term
n2 = App ts $ App ts tz

n1 :: Term
n1 = App ts tz

n3 :: Term
n3 = App ts (App ts (App ts tz))

term :: Term
term = App tconst $ App (App (App (Abs "f" $ Abs "x" $ Abs "y" $ Abs "z" $ Var "f") (Atom "f")) (Atom "x")) (Atom "y")

main :: IO ()
main =
  let (State t _ _) = steps $ start term in
  putStrLn $ show t
