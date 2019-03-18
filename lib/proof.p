type Eq a b = forall f. f a -> f b
let refl = Eq \x -> x

type Coerce t = t
let coerce q a = unCoerce <| unEq q (Coerce a)

type Symm p a b = p b a
let symm q = unSymm <| unEq q (Symm refl)

let flip f x y = f y x
let trans = flip unEq : forall a b c. Eq a b -> Eq b c -> Eq a c

type Lift f a b = Eq (f a) (f b)
let lift q = unLift <| unEq q (Lift refl)

