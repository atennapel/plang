; some primitives so we have some concrete values
decltype PrimBool : Type
declare primTrue : PrimBool
foreign primTrue "true"
declare primFalse : PrimBool
foreign primFalse "false"

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

; our base types
type Void = forall t. t

type Unit = forall t. t -> t
let unit = Unit \x -> x

type Pair a b = forall r. (a -> b -> r) -> r
let pair a b = Pair \f -> f a b
let fst p = unPair p \x y -> x
let snd p = unPair p \x y -> y

type Sum a b = forall r. (a -> r) -> (b -> r) -> r
let inl x = Sum \f g -> f x
let inr x = Sum \f g -> g x

type Bool = forall r. r -> r -> r
let true = Bool \a b -> a
let false = Bool \a b -> b
let cond c a b = unBool c a b
let if c a b = cond c a b unit
