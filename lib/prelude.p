id = \x -> x
const = \x y -> x
constid = const id
comp = \f g x -> f (g x)
dup = \f x -> f x x
flip = \f x y -> f y x
fork = \f g h x -> f (g x) (h x)

Void = t
void = \Void x -> x

Unit = t -> t
unit = Unit id

Pair = \a b. (a -> b -> c) -> c
unPair = \Pair f -> f
pair = \a b -> Pair \f -> f a b
fst = \p -> unPair p const
snd = \p -> unPair p constid
curry = \f x y -> f (pair x y)
uncurry = \f p -> unPair p f

Sum = \a b. (a -> c) -> (b -> c) -> c
unSum = \Sum f -> f
inl = \x -> Sum \f g -> f x
inr = \x -> Sum \f g -> g x
caseSum = \f g s -> unSum s f g

Maybe = \t. Sum Unit t
unMaybe = \Maybe s -> s
nothing = Maybe (inl unit)
just = \x -> Maybe (inr x)
caseMaybe = \m -> unSum (unMaybe m)

Bool = Sum Unit Unit
unBool = \Bool s -> s
true = Bool (inl unit)
false = Bool (inr unit)
if = \c a b -> caseSum a b (unBool c)
cond = \c a b -> if c (\x -> a) (\x -> b)

Nat = Sum Unit Nat
unNat = \Nat s -> s
z = Nat (inl unit)
s = \n -> Nat (inr n)
0 = z
1 = s 0
2 = s 1
3 = s 2
4 = s 3
5 = s 4
6 = s 5
7 = s 6
8 = s 7
9 = s 8
caseNat = \z s n -> caseSum z s (unNat n)
iterNat = \z s n -> caseSum z (\n -> s (iterNat z s n)) (unNat n)
recNat = \z s n -> caseSum z (\n -> s n (recNat z s n)) (unNat n)
showNat = \n s z -> caseSum (const z) (\n -> s (showNat n s z)) (unNat n)
isZero = caseSum (const true) (const false)
isSucc = caseSum (const false) (const true)
pred = caseNat (const z) id
add = \a b -> caseSum (const b) (\n -> add n b) (unNat a)
mul = \a b -> caseSum (const z) (\n -> add b (mul n b)) (unNat a) 
