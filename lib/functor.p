import basic

type Functor f = forall a b. (a -> b) -> f a -> f b
let map (Functor f) = f

let pairFunctor = Functor \m p -> casePair p \x y -> pair x (m y)
let sumFunctor = Functor \m s -> caseSum s (\x -> inl x) (\y -> inr (m y))
