; scott-encoded lists
; uses unsafeFix
import combinators
import basic
import bool
import monoid
import functor
import nat

type List t = forall r. (() -> r) -> (t -> List t -> r) -> r
let caseList (List f) = f

let foldr f i = unsafeFix \rec l ->
  caseList l (\() -> i) (\h t -> f h (rec t))

let nil = List \fn fc -> fn ()
let cons h t = List \fn fc -> fc h t

let tail l = caseList l (\() -> nil) (\_ t -> t)

let isEmpty l = caseList l (\() -> true) (\_ _ -> false)
let isNonEmpty l = not (isEmpty l)

let wrap x = cons x nil

let append = flip (foldr cons)
let reverse l = (unsafeFix \rec l a -> caseList l (\() -> a) (\h t -> rec t (cons h a))) l nil

let monoidList = monoid nil append
let fold m = foldr (mappend m) (munit m)

let mapList f l = foldr (\h r -> cons (f h) r) nil l
let functorList = Functor mapList

let repeat n x = iterNat n (cons x) nil
let range n = reverse (recNat n (\n r -> cons n r) nil)

let sum = foldr add zero
let product = foldr mul one
