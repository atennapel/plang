import combinators

type Leibniz a b = forall f. f a -> f b

let refl = Leibniz \x -> x
  : forall t. Leibniz t t

let trans = flip (\(Leibniz f) -> f)
  : forall a b c. Leibniz a b -> Leibniz b c -> Leibniz a c

type Id t = t
let coerce = \q a -> (\(Id x) -> x) ((\(Leibniz x) -> x) q (Id a))
  : forall a b. Leibniz a b -> a -> b

type Symm p a b = p b a
let symm = \q -> (\(Symm x) -> x) ((\(Leibniz x) -> x) q (Symm refl))

type Lift f a b = Leibniz (f a) (f b)
let lift = \q -> (\(Lift x) -> x) ((\(Leibniz x) -> x) q (Lift refl))
  : forall f a b. Leibniz a b -> Leibniz (f a) (f b)
