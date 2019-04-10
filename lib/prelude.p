import combinators
import basic
import bool

type Nat = forall t. (t -> t) -> t -> t
let foldNat (Nat f) = f
let cataNat (Nat f) = f

let z = Nat \f x -> x
let s n = Nat \f x -> f (foldNat n f x)

let isZero (Nat f) = f (\-> false) true

let pow (a:Nat) (b:Nat) = Nat <| (foldNat b) (foldNat a)

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

type Eq a b = forall f. f a -> f b

let refl = Eq \x -> x
  : forall t. Eq t t

let trans = flip (\(Eq f) -> f)
  : forall a b c. Eq a b -> Eq b c -> Eq a c

type Id t = t
let coerce = \q a -> (\(Id x) -> x) ((\(Eq x) -> x) q (Id a))
  : forall a b. Eq a b -> a -> b

type Symm p a b = p b a
let symm = \q -> (\(Symm x) -> x) ((\(Eq x) -> x) q (Symm refl))

type Lift f a b = Eq (f a) (f b)
let lift = \q -> (\(Lift x) -> x) ((\(Eq x) -> x) q (Lift refl))
  : forall f a b. Eq a b -> Eq (f a) (f b)


