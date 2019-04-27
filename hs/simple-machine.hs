import Debug.Trace (trace)

type Ix = Int
type Const = String

data Term
  = Var Ix
  | Abs Term
  | App Term Term
  | Const Const
  | Pair Val Val
  | PairC Term Term

instance Show Term where
  show (Var i) = show i
  show (Abs b) = "(\\" ++ (show b) ++ ")"
  show (App a b) = "(" ++ (show a) ++ " " ++ (show b) ++ ")"
  show (Const x) = x
  show (Pair a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
  show (PairC a b) = "(PairC " ++ (show a) ++ " " ++ (show b) ++ ")"

data Val
  = VClos Term Env
  | VPair Val Val
  | VConst Const

instance Show Val where
  show (VClos t e) = "\\" ++ (show t) ++ "@" ++ (show e)
  show (VPair a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
  show (VConst x) = x

type Env = [Val]

data Cont
  = Top
  | Arg Term Env Cont
  | Fun Term Env Cont
  | CLeft Term Env Cont
  | CRight Val Cont
  deriving (Show)

data State = State Term Env Cont
  deriving (Show)

makeVal :: Term -> Env -> Maybe Val
makeVal (Abs t) e = return $ VClos t e
makeVal (Pair a b) _ = return $ VPair a b
makeVal (Const x) _ = return $ VConst x
makeVal _ _ = Nothing

step :: State -> Maybe State
step (State (Var i) e k) =
  case e !! i of
    VClos v e' -> return $ State (Abs v) e' k
    VPair a b -> return $ State (Pair a b) e k
    VConst x -> return $ State (Const x) e k
step (State (App a b) e k) = return $ State a e (Arg b e k)
step (State (PairC a b) e k) = return $ State a e (CLeft b e k)
step (State (Abs v) e (Arg a e' k)) = return $ State a e' (Fun v e k)
step (State v e (CLeft a e' k)) = do
  val <- makeVal v e
  return $ State a e' (CRight val k)
step (State v e (Fun v' e' k)) = do
  val <- makeVal v e
  return $ State v' (val : e') k
step (State v e (CRight v' k)) = do
  val <- makeVal v e
  return $ State (Pair v' val) e k
step _ = Nothing

steps :: State -> State
steps st =
  case trace (show st) $ step st of
    Just st' -> steps st'
    Nothing -> st

initial :: Term -> State
initial t = State t [] Top

run :: Term -> Maybe Val
run t =
  case steps (initial t) of
    State v e Top -> makeVal v e
    _ -> Nothing
     

showsteps  :: State -> IO ()
showsteps st = do
  putStrLn (show st)
  case trace (show st) $ step st of
    Just st' -> showsteps st'
    Nothing -> return ()

tI, tK, tS :: Term
tI = Abs (Var 0)
tK = Abs (Abs (Var 1))
tS = Abs (Abs (Abs (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0)))))

tz, ts :: Term
tz = Abs (Abs (Var 0))
ts = Abs (Abs (Abs (App (Var 1) (App (App (Var 2) (Var 1)) (Var 0)))))

tn :: Term
tn = App ts $ App ts $ App ts tz

reify :: Term -> Term
reify t = App (App t (Abs (PairC (Const "s") (Var 0)))) (Const "z")

omega :: Term
omega = App (Abs (App (Var 0) (Var 0))) (Abs (App (Var 0) (Var 0)))

term :: Term
term = reify tn

main :: IO ()
main = putStrLn $ show $ run term
