; parigot-encoded binary natural numbers
import combinators
import basic
import bool
import monoid

type BNat = forall t. (() -> t) -> (BNat -> (() -> t) -> t) -> (BNat -> (() -> t) -> t) -> t
let recBNat (BNat f) = f
let caseBNat n fz ft fti = recBNat n fz (\n _ -> ft n) (\n _ -> fti n)
let cataBNat n fz ft fti = recBNat n (\() -> fz) (\_ r -> ft (r ())) (\_ r -> fti (r ()))

let bz = BNat \fz ft fti -> fz ()
let bt n = BNat \fz ft fti -> ft n (\() -> recBNat n fz ft fti)
let bti n = BNat \fz ft fti -> fti n (\() -> recBNat n fz ft fti)

let zero = bz
let one = bti bz
let twice = bt
let twicePlusOne = bti

let div2 n = caseBNat n (\() -> bz) (\n -> n) (\n -> n) 

let pred n = recBNat n (\() -> bz) (\_ r -> bti (r ())) (\n _ -> caseBNat n (\() -> bz) (\_ -> bt n) (\_ -> bt n))
let succ n = recBNat n (\() -> bti bz) (\n _ -> bti n) (\_ r -> bt (r ()))

let iterBNat n f x = recBNat n (\() -> id) (\_ r -> let rr = r () in comp rr rr) (\_ r -> let rr = r () in comp3 f rr rr) x

let add n m = iterBNat n succ m
let mul n m = iterBNat n (add m) bz
let sub n m = iterBNat m pred n
let pow n m = iterBNat m (mul n) (bti bz)

let monoidAdd = monoid bz add
let monoidMul = monoid (bti bz) mul

let isZero n = caseBNat n (\() -> true) (\_ -> true) (\_ -> false)
let isPositive n = not (isZero n)

let lteq n m = isZero (sub n m)
let gteq n m = isZero (sub m n)
let gt n m = not (lteq n m)
let lt n m = not (gteq n m)

let eq n m = and (lteq n m) (lteq m n)

let isEven n = iterBNat n not true
let isOdd n = not (isEven n)

let divmod n m =
  if isZero m then
    pair bz bz
  else
    iterBNat n
      (\r ->
        if lt (snd r) m then
          r
        else
          pair (succ (fst r)) (sub (snd r) m))
      (pair bz n)
let div n m = fst (divmod n m)
let mod n m = snd (divmod n m)
