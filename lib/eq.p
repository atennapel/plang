type Eq a b = forall f. f a -> f b

let refl = Eq \x -> x
  : forall t. Eq t t

let flip = \f x y -> f y x

let trans = flip (\(Eq f) -> f)
  : forall a b c. Eq a b -> Eq b c -> Eq a c

type Id t = t
let coerce = \q a -> (\(Id x) -> x) ((\(Eq x) -> x) q (Id a))
  : forall a b. Eq a b -> a -> b

type Symm p a b = p b a
let symm = \q -> (\(Symm x) -> x) ((\(Eq x) -> x) q (Symm refl))

type Lift f a b = Eq (f a) (f b)
let lift = \q -> (\(Lift x) -> x) ((\(Eq x) -> x) q (Lift refl))
  : forall f a b. Eq a b -> Eq (f a)

