type Nat = forall t. (t -> t) -> t -> t
let unNat (Nat f) = f

let z = Nat \f x -> x
let s n = Nat \f x -> f (unNat n f x)

let n0 = z
let n1 = s n0
let n2 = s n1
let n3 = s n2
let n4 = s n3
let n5 = s n4

let pow (a:Nat) (b:Nat) = Nat <| (unNat b) (unNat a)

