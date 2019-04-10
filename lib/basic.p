type Void = forall t. t
let caseVoid (Void f) = f

type Unit = forall t. t -> t
let unit = Unit \x -> x

type Pair a b = forall t. (a -> b -> t) -> t
let casePair (Pair f) = f
let pair a b = Pair \f -> f a b
let fst p = casePair p \a b -> a
let snd p = casePair p \a b -> b

type Sum a b = forall t. (a -> t) -> (b -> t) -> t
let caseSum (Sum f) = f
let inl x = Sum \f g -> f x
let inr x = Sum \f g -> g x
