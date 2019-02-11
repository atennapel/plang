Nat = (Nat -> t -> t) -> t -> t

z = Nat \f x -> x
s = Nat 

main = z
