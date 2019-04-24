import basic
import nat

; integer k is (a, b) where k = a - b
type Int = Pair Nat Nat
let caseInt (Int (Pair f)) = f
let caseInt2 (Int (Pair f)) (Int (Pair g)) h =
  f \a b -> g \c d -> h a b c d

let makeInt a b = Int (pair a b)

let nat2int n = makeInt n zero
let int2nat n = caseInt n \a b -> sub a b

let neg n = caseInt n \a b -> makeInt b a
let addi a b = caseInt2 a b \a b c d -> makeInt (add a c) (add b d)
let subi a b = addi a (neg b)
let muli a b = caseInt2 a b \a b c d -> makeInt (add (mul a c) (mul b d)) (add (mul a d) (mul b c))  
; TODO divi modi divmodi
; TODO comparisons
