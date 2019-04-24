; parigot-encoded binary natural numbers

import combinators
import basic
import bool
import monoid

type Nat = forall t. (() -> t) -> (Nat -> (() -> t) -> t) -> (Nat -> (() -> t) -> t) -> t
let recNat (Nat f) = f
let caseNat n fz ft fti = recNat n fz (\n _ -> ft n) (\n _ -> fti n)
let cataNat n fz ft fti = recNat n (\() -> fz) (\_ r -> ft (r ())) (\_ r -> fti (r ()))

let bz = Nat \fz ft fti -> fz ()
let bt n = Nat \fz ft fti -> ft n (\() -> recNat n fz ft fti)
let bti n = Nat \fz ft fti -> fti n (\() -> recNat n fz ft fti)

let zero = bz
let one = bti bz
let twice = bt
let twicePlusOne = bti

let div2 n = caseNat n (\() -> bz) (\n -> n) (\n -> n) 

let pred n = recNat n (\() -> bz) (\_ r -> bti (r ())) (\n _ -> caseNat n (\() -> bz) (\_ -> bt n) (\_ -> bt n))
let succ n = recNat n (\() -> bti bz) (\n _ -> bti n) (\_ r -> bt (r ()))

let iterNat n f x = recNat n (\() -> id) (\_ r -> let rr = r () in comp rr rr) (\_ r -> let rr = r () in comp3 f rr rr) x
let recCNat n f x = snd (iterNat n (\r -> let m = fst r in pair (succ m) (f m (snd r))) (pair bz x))

let add n m = iterNat n succ m
let mul n m = iterNat n (add m) bz
let sub n m = iterNat m pred n
let pow n m = iterNat m (mul n) (bti bz)

let monoidAdd = monoid bz add
let monoidMul = monoid (bti bz) mul

let isZero n = recNat n (\() -> true) (\_ r -> r ()) (\_ _ -> false)
let isPositive n = not (isZero n)

let lteq n m = isZero (sub n m)
let gteq n m = isZero (sub m n)
let gt n m = not (lteq n m)
let lt n m = not (gteq n m)

let eq n m = and (lteq n m) (lteq m n)

let isEven n = iterNat n not true
let isOdd n = not (isEven n)

let divmod n m =
  if isZero m then
    pair bz bz
  else
    iterNat n
      (\r ->
        if lt (snd r) m then
          r
        else
          pair (succ (fst r)) (sub (snd r) m))
      (pair bz n)
let div n m = fst (divmod n m)
let mod n m = snd (divmod n m)
