; parigot-encoded lists
import combinators
import basic
import bool
import monoid
import functor
import nat

type List t = forall r. (() -> r) -> (t -> List t -> (() -> r) -> r) -> r
let recList (List f) = f
let caseList l fn fc = recList l fn (\h t _ -> fc h t)
let cataList l fn fc = recList l (\() -> fn) (\h _ r -> fc h (r ()))

let nil = List \fn fc -> fn ()
let cons h t = List \fn fc -> fc h t (\() -> recList t fn fc)

let tail l = caseList l (\() -> nil) (\_ t -> t)

let isEmpty l = caseList l (\() -> true) (\_ _ -> false)
let isNonEmpty l = not (isEmpty l)

let wrap x = cons x nil

let foldr f i l = cataList l i f
let append = flip (foldr cons)
let reverse = foldr (\h r -> append r (wrap h)) nil

let monoidList = monoid nil append
let fold m = foldr (mappend m) (munit m)

let mapList f l = foldr (\h r -> cons (f h) r) nil l
let functorList = Functor mapList

let repeat n x = iterNat n (cons x) nil
let range n = reverse (recCNat n (\n r -> cons n r) nil)

let sum = foldr add zero
let product = foldr mul one

