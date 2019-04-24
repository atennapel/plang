import combinators
import basic
import bool
import monoid
import nat

; integer k is (a, b) where k = a - b
type Int = Pair Nat Nat
let caseInt (Int (Pair f)) = f
let caseInt2 (Int (Pair f)) (Int (Pair g)) h =
  f \a b -> g \c d -> h a b c d

let makeInt a b = Int (pair a b)

let nat2int n = makeInt n zero
let int2nat n = caseInt n \a b -> sub a b

let zeroi = nat2int zero
let onei = nat2int one
let twicei n = caseInt n \a b -> makeInt (twice a) (twice b)
let twicePlusOnei n = caseInt n \a b -> makeInt (twicePlusOne a) (twice b)
let div2i n = caseInt n \a b -> makeInt (div2 a) (div2 b)
let succi n = caseInt n \a b -> makeInt (succ a) b
let predi n = caseInt n \a b -> makeInt a (succ b)

let isEveni n = caseInt n \a b -> eqb (isEven a) (isEven b)
let isNoti n = not (isEveni n)

let negi n = caseInt n \a b -> makeInt b a
let addi a b = caseInt2 a b \a b c d -> makeInt (add a c) (add b d)
let subi a b = addi a (negi b)
let muli a b = caseInt2 a b \a b c d -> makeInt (add (mul a c) (mul b d)) (add (mul a d) (mul b c))

let divi a b = caseInt2 a b \a b c d ->
  (let m = sub c d in
  if isZero m then
    let x = sub d c in
    negi (makeInt (div a x) (div b x))
  else
    makeInt (div a m) (div b m))
let remi n m = subi n (muli (divi n m) m)
let divremi n m =
  (let d = divi n m in pair d (subi n (muli d m)))
let modi n m = remi (addi (remi n m) m) m

let monoidAddi = monoid zeroi addi
let monoidMuli = monoid onei muli

let sqi n = muli n n

let powi n = unsafeFix \rec m ->
  caseBNat m
    (\() -> onei)
    (\mm -> sqi (rec mm))
    (\mm -> muli n (sqi (rec mm)))

let isZeroi n = caseInt n eq
let isNonZeroi n = not (isZeroi n)
let lteqi n m = isZeroi (subi n m)
let gteqi n m = isZeroi (subi m n)
let gti n m = not (lteqi n m)
let lti n m = not (gteqi n m)
let eqi n m = and (lteqi n m) (lteqi m n)
let isNegativei n = lti n zeroi
let isPositivei n = gti n zeroi
