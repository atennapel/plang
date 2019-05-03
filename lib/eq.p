import bool

type Eq t = t -> t -> Bool
let eq (Eq f) = f
let neq q a b = not (eq q a b)

let eqBool = Eq eqb
