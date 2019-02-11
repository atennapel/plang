id = \x -> x
const = \x y -> x
constid = const id
comp = \f g x -> f (g x)
dup = \f x -> f x x
flip = \f x y -> f y x
fork = \f g h x -> f (g x) (h x)

Void = t
void = \Void x -> x

Unit = t -> t
unit = Unit id

Bool = t -> t -> t
unBool = \Bool f -> f
true = Bool \a b -> a
false = Bool \a b -> b
cond = \c a b -> unBool c a b
if = \c a b -> (cond c a b) unit

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

