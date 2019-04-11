import combinators
import basic
import bool
import nat

type Char = Nat
let fromChar (Char n) = n

type List t = forall r. r -> (t -> r -> r) -> r
let caseList (List f) = f
let cataList (List f) = f
let nil = List \n c -> n
let cons h t = List \n c -> c h (caseList t n c)
let isNil l = caseList l true (\_ _ -> false)
let foldr f i l = caseList l i f
let append = flip (foldr cons)

type Str = List Char
let fromStr (Str l) = l
let strLift2 f (Str a) (Str b) = Str (f a b)
let strAppend = strLift2 append
