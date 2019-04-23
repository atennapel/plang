; scott encoded unary natural numbers
; uses unsafeFix
import basic
import nat

type ScottNat = forall t. (() -> t) -> (ScottNat -> t) -> t
let caseScottNat (ScottNat f) = f

let z = ScottNat \z s -> z ()
let s n = ScottNat \z s -> s n

let predSN n = caseScottNat n (\() -> z) (\n -> n)

let iterScottNat = unsafeFix \rec n f x ->
  caseScottNat n
    (\() -> x)
    (\m -> rec m f (f x))

let addSN a b = iterScottNat a s b
let mulSN a b = iterScottNat a (addSN b) z

let fromNat n = iterNat n s z
let toNat n = iterScottNat n succ zero
