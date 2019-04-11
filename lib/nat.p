; parigot-encoded unary natural numbers
import basic
import bool

type Nat = forall t. (() -> t) -> (Nat -> t -> t) -> t
let recNat (Nat f) = f
let caseNat n fz fs = recNat n fz (\n _ -> fs n)
let cataNat n fs fz = recNat n (\() -> fz) (\_ r -> fs r)

let z = Nat \fz fs -> fz ()
let s n = Nat \fz fs -> fs n (recNat n fz fs)

let pred n = caseNat n (\() -> z) (\m -> m)

let add n m = cataNat n s m
let mul n m = cataNat n (add m) z
let sub n m = cataNat m pred n
let pow n m = cataNat m (mul n) (s z)

let isZero n = caseNat n (\() -> true) (\_ -> false)
let isPositive n = not (isZero n)

let lteq n m = isZero (sub n m)
let gteq n m = isZero (sub m n)
let gt n m = not (lteq n m)
let lt n m = not (gteq n m)

let eq n m = and (lteq n m) (lteq m n)

let isEven n = cataNat n not true
let isOdd n = not (isEven n)

let divmod n m =
  if isZero m then
    pair z z
  else
    recNat n
      (\() -> pair z n)
      (\_ r ->
        if lt (snd r) m then
          r
        else
          pair (s (fst r)) (sub (snd r) m))
let div n m = fst (divmod n m)
let mod n m = snd (divmod n m)
