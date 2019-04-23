import combinators
import basic
import bool

type Nat = forall t. (() -> t) -> (Nat -> t) -> (Nat -> t) -> t
let caseBNat (Nat f) = f

let BZ = Nat \z t ti -> z ()
let BT n = Nat \z t ti -> t n
let BTI n = Nat \z t ti -> ti n

let isZero n = caseBNat n (\() -> true) (\_ -> false) (\_ -> false)
let isPositive n = not (isZero n)

let recBNat = unsafeFix \rec n fz ft fti ->
  caseBNat n fz (\m -> ft m (\() -> rec m fz ft fti)) (\m -> fti m (\() -> rec m fz ft fti))
let iterBNat n fz ft fti = recBNat n fz (\_ -> ft) (\_ -> fti)

let zero = BZ
let one = BTI BZ
let twice = BT
let twicePlusOne = BTI

let div2 n = caseBNat n (\() -> zero) id id

let succ n = recBNat n (\() -> one) (\n _ -> BTI n) (\_ r -> BT (r ()))
let pred n = recBNat n (\() -> zero) (\_ r -> BTI (r ())) (\n _ -> BT n)

let iterNat n f x = recBNat n (\() -> id) (\_ r -> let rr = r () in comp rr rr) (\_ r -> let rr = r () in comp3 f rr rr) x
let recNat n f x = snd (iterNat n (\r -> let m = fst r in pair (succ m) (f m (snd r))) (pair BZ x))

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