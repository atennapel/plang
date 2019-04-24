import basic
import nat
import int

; rational number q is (k, a) where q = k / (1 + a)
type Rat = Pair Int Nat
let caseRat (Rat (Pair f)) = f

let makeRat a b = Rat (pair a b)
let rat a b = makeRat a (pred b)

let nat2rat n = makeRat (nat2int n) zero
let int2rat n = makeRat n zero
