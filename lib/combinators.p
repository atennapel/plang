let id x = x
let const x y = x
let flip f x y = f y x
let fork f g h x = f (g x) (h x)
let dup f x = f x x
