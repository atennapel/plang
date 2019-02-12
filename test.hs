id = \x -> x
const = \x y -> x
constid = const id

Unit = t -> t
unit = Unit id

Pair = \a b. (a -> b -> c) -> c
unPair = \Pair f -> f
pair = \a b -> Pair \f -> f a b
fst = \p -> unPair p const
snd = \p -> unPair p constid

Sum = \a b. (a -> c) -> (b -> c) -> c
unSum = \Sum f -> f
inl = \x -> Sum \f g -> f x
inr = \x -> Sum \f g -> g x
caseSum = \f g s -> unSum s f g

Mu = \f. (f a -> a) -> a
unMu = \Mu f -> f

ListF = \t r. Sum Unit (Pair t r)
unListF = \ListF s -> s
List = \t. Mu (ListF t)
unList = \List m -> m
nil = List (Mu (\f -> f (ListF (inl unit))))
cons = \h t -> List (Mu (\f -> f (ListF (inr (pair h (unMu (unList t) f))))))

main = cons
