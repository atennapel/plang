Nat = (t -> t) -> t -> t
un

id = \x -> x
compose = \f g x -> f (g x)
const = \x y -> x
flip = \f x y -> f y x
dup = \f x -> f x x

z = Nat \f x -> x
s = \n -> Nat \f x -> (\Nat n -> f (n f x)) n

main = s z
