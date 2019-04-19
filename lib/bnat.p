; parigot-encoded binary natural numbers
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
