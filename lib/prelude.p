; some primitives so we have some concrete values
declare True : Bool
foreign True "true"
declare False : Bool
foreign False "false"
let not b = if b then False else True
let and a b = if a then b else False
let or a b = if a then True else b

decltype PrimNat : Type
declare primZ : PrimNat
foreign primZ "0"
declare primS : PrimNat -> PrimNat
foreign primS "x => x + 1"

decltype PrimList : Type -> Type
declare primNil : forall t. PrimList t
foreign primNil "[]"
declare primCons : forall t. t -> PrimList t -> PrimList t
foreign primCons "h => t => [h].concat(t)"

decltype Pair : Type -> Type -> Type
declare pair : forall a b. a -> b -> Pair a b
foreign pair "a => b => [a, b]"
declare fst : forall a b. Pair a b -> a
foreign fst "p => p[0]"
declare snd : forall a b. Pair a b -> b
foreign snd "p => p[1]"

; our base types
type Void = forall t. t

type Unit = forall t. t -> t
let unit = Unit \x -> x

type Sum a b = forall r. (a -> r) -> (b -> r) -> r
let inl x = Sum \f g -> f x
let inr x = Sum \f g -> g x

type List t = forall r. r -> (t -> r -> r) -> r
let Nil = List \n c -> n
let Cons h t = List \n c -> c h (unList t n c)
let showList l = unList l primNil primCons

let foldl f v l = unList l v f
let mapList f = foldl (\h t -> Cons (f h) t) Nil

type Functor f = forall a b. (a -> b) -> f a -> f b
let map = unFunctor

let ArrFunctor = Functor \f g x -> f (g x)
let ListFunctor = Functor mapList
