; scott-encoded binary natural numbers
; uses unsafeFix

import combinators
import basic
import bool
import monoid

type Nat = forall t. (() -> t) -> (Nat -> t) -> (Nat -> t) -> t
let caseBNat (Nat f) = f

let BZ = Nat \z t ti -> z ()
let unsafeBT n = Nat \z t ti -> t n
let BT n = caseBNat n (\() -> BZ) (\_ -> unsafeBT n) (\_ -> unsafeBT n)
let BTI n = Nat \z t ti -> ti n

let isZero n = caseBNat n (\() -> true) (\_ -> false) (\_ -> false)
let isPositive n = not (isZero n)

let recBNat = unsafeFix \rec n fz ft fti ->
  caseBNat n fz (\m -> ft m (\() -> rec m fz ft fti)) (\m -> fti m (\() -> rec m fz ft fti))
let iterBNat n fz ft fti = recBNat n fz (\_ -> ft) (\_ -> fti)

; for reification
let bz = BZ
let bt = BT
let bti = BTI
let cataNat n fz ft fti = recBNat n (\() -> fz) (\_ r -> ft (r ())) (\_ r -> fti (r ()))
; end

let zero = BZ
let one = BTI BZ
let twice = BT
let twicePlusOne = BTI

let div2 n = caseBNat n (\() -> zero) id id

let succ n = recBNat n (\() -> one) (\n _ -> BTI n) (\_ r -> BT (r ()))
let pred n = recBNat n (\() -> zero) (\_ r -> BTI (r ())) (\n _ -> BT n)

let iterNat n f x = recBNat n (\() -> id) (\_ r -> let rr = r () in comp rr rr) (\_ r -> let rr = r () in comp3 f rr rr) x
let recNat n f x = snd (iterNat n (\r -> let m = fst r in pair (succ m) (f m (snd r))) (pair BZ x))

let isEven n = caseBNat n (\() -> true) (\_ -> true) (\_ -> false)
let isOdd n = not (isEven n)

let add = unsafeFix \rec n m ->
  caseBNat n
    (\() -> m)
    (\nn -> caseBNat m
      (\() -> n)
      (\mm -> BT (rec nn mm))
      (\mm -> BTI (rec nn mm)))
    (\nn -> caseBNat m
      (\() -> n)
      (\mm -> BTI (rec nn mm))
      (\mm -> succ (BTI (rec nn mm))))

let mul = unsafeFix \rec n m ->
  caseBNat n
    (\() -> BZ)
    (\nn -> caseBNat m
      (\() -> BZ)
      (\mm -> BT (BT (rec nn mm)))
      (\mm -> BT (rec nn m)))
    (\nn -> caseBNat m
      (\() -> BZ)
      (\mm -> BT (rec mm n))
      (\mm -> add m (add (BT nn) (BT (BT (rec nn mm))))))

let monoidAdd = monoid zero add
let monoidMul = monoid one mul

let sq n = mul n n
let pow2 n = iterNat n unsafeBT one
let fib n = fst (iterNat n (\r -> let m = snd r in pair m (add (fst r) m)) (pair zero one))
let fac n = recNat n (\n r -> mul (succ n) r) one

let sub = unsafeFix \rec n m ->
  caseBNat n
    (\() -> BZ)
    (\nn -> caseBNat m
      (\() -> n)
      (\mm -> BT (rec nn mm))
      (\mm -> pred (BT (rec nn mm))))
    (\nn -> caseBNat m
      (\() -> n)
      (\mm -> pred (BT (rec (succ nn) mm)))
      (\mm -> BT (rec nn mm)))

let pow n = unsafeFix \rec m ->
  caseBNat m
    (\() -> one)
    (\mm -> sq (rec mm))
    (\mm -> mul n (sq (rec mm)))

let lteq n m = isZero (sub n m)
let gteq n m = isZero (sub m n)
let gt n m = not (lteq n m)
let lt n m = not (gteq n m)

let eq = unsafeFix \rec n m ->
  caseBNat n
    (\() -> isZero m)
    (\nn -> caseBNat m
      (\() -> false)
      (\mm -> rec nn mm)
      (\mm -> false))
    (\nn -> caseBNat m
      (\() -> false)
      (\mm -> false)
      (\mm -> rec nn mm))

let divmod n m =
  if isZero m then
    pair zero zero
  else
    iterNat n
      (\r ->
        if lt (snd r) m then
          r
        else
          pair (succ (fst r)) (sub (snd r) m))
      (pair zero n)
let div n m = fst (divmod n m)
let mod n m = snd (divmod n m)
