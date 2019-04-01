type Void = forall t. t
let caseVoid (Void f) = f

type Unit = forall t. t -> t
let unit = Unit \x -> x

type Pair a b = forall t. (a -> b -> t) -> t
let casePair (Pair f) = f
let pair a b = Pair \f -> f a b
let fst p = unPair p \a b -> a
let snd p = unPair p \a b -> b

type Sum a b = forall t. (a -> t) -> (b -> t) -> t
let caseSum (Sum f) = f
let inl x = Sum \f g -> f x
let inr x = Sum \f g -> g x

type Bool = forall t. t -> t -> t
let cond (Bool f) = f
let true = Bool \a b -> a
let false = Bool \a b -> b
let if c a b = (cond c a b) unit

type Nat = forall t. (t -> t) -> t -> t
let unNat (Nat f) = f

let z = Nat \f x -> x
let s n = Nat \f x -> f (unNat n f x)

let isZero (Nat f) = f (\-> false) true

let n0 = z
let n1 = s n0
let n2 = s n1
let n3 = s n2
let n4 = s n3
let n5 = s n4

let pow (a:Nat) (b:Nat) = Nat <| (unNat b) (unNat a)

type List t = forall r. r -> (t -> r -> r) -> r
let caseList (List f) = f
let nil = List \n c -> n
let cons h t = List \n c -> c h (caseList t n c)
let foldr f i l = caseList l i f

let Str = List Nat
let unStr (Str l) = l

