module Main exposing (..)

import Html exposing (text)

type alias Label = String

type Val
  = VUnit
  | VInt Int
  | VString String
  | VMaybe (Maybe Val)

type Eff t
  = Return t
  | Cont Label Val (Val -> Eff t)

pure e =
  case e of
    Return v -> Just v
    Cont _ _ _ -> Nothing

perform l v = Cont l v Return

do : Eff a -> (a -> Eff a) -> Eff a
do e f =
  case e of
    Return v -> f v
    Cont l v c -> Cont l v (\y -> do (c y) f)

finally f e = do e f

handle : Label -> (Val -> (Val -> Eff t) -> Eff t) -> Eff t -> Eff t
handle l h e =
  case e of
    Return _ as r -> r
    Cont cl v c ->
      if l == cl then
        h v (\x -> handle l h (c x))
      else
        Cont cl v (\x -> handle l h (c x))

getInt v =
  case v of
    VInt n -> Just n
    _ -> Nothing 

mapMaybe : (t -> Val) -> (Maybe t -> Val)
mapMaybe f m = VMaybe (Maybe.map f m)

mapInt2 : (Int -> Int -> Int) -> Val -> Val -> Val
mapInt2 f a b = mapMaybe VInt <| Maybe.map2 f (getInt a) (getInt b)

t =
  do (perform "Get" VUnit) <| \x ->
  do (perform "Set" (VInt 100)) <| \_ ->
  do (perform "Get" VUnit) <| \y ->
  Return (mapInt2 (+) x y)

main = text <| toString (handle "Set" (\v k -> k v) <| handle "Get" (\v k -> k (VInt 10)) t)
