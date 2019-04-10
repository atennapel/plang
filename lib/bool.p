type Bool = forall t. t -> t -> t
let cond (Bool f) = f
let true = Bool \a b -> a
let false = Bool \a b -> b
let if c a b = (cond c a b) unit

let not b = if b then false else true
let or a b = if a then true else b
let and a b = if a then b else false
