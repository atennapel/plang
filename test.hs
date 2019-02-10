BoxT = \t. t

id = \x -> x
compose = \f g x -> f (g x)
const = \x y -> x
flip = \f x y -> f y x
dup = \f x -> f x x

z = \f x -> x
s = \n f x -> f (n f x)

main = BoxT z
