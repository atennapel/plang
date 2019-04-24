import basic

type Iso a b = Pair (a -> b) (b -> a)
let to (Iso p) = fst p
let from (Iso p) = snd p
let makeIso a b = Iso (pair a b)
