import basic

type Monoid t = Pair t (t -> t -> t)
let monoid u a = Monoid (pair u a)
let munit (Monoid p) = fst p
let mappend (Monoid p) = snd p
