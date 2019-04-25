import combinators
import basic
import bool
import monoid
import nat
import int

; rational number q is (k, a) where q = k / a
type Rat = Pair Int Nat
let caseRat (Rat (Pair f)) = f
let caseRat2 (Rat (Pair f)) (Rat (Pair g)) h =
  f \a b -> g \c d -> h a b c d
let makeRat a b = Rat (pair a b)

let simplifyr n = caseRat n \a b ->
  (let s = spliti a in
   let g = gcd (snd s) b in
   makeRat (negifnat (fst s) (div (snd s) g)) (div b g))
let rat a b = simplifyr (makeRat a b)

let nat2rat n = makeRat (nat2int n) one
let int2rat n = makeRat n one

let zeror = makeRat zeroi one
let oner = makeRat onei one
let twicer n = caseRat n \a b -> rat (twicei a) b
let twicePlusOner n = caseRat n \a b -> rat (twicePlusOnei a) b
let div2r n = caseRat n \a b -> rat a (twice b)

let negr n = caseRat n \a b -> makeRat (negi a) b
let negifr b n = if b then negr n else n
let negoner = negr oner

let addr a b = caseRat2 a b \a b c d ->
  rat (addi (muli a (nat2int d)) (muli c (nat2int b))) (mul b d)
let succr n = addr n oner
let decr n = addr n negoner
let subr a b = addr a (negr b)
let mulr a b = caseRat2 a b \a b c d ->
  rat (muli a c) (mul b d)

