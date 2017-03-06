;; Num
type Num [t] {
  Num [(Rec {
    neg (Arr t t)
    add (Arr t (Arr t t))
    sub (Arr t (Arr t t))
    mul (Arr t (Arr t t))
    div (Arr t (Arr t t))
    rem (Arr t (Arr t t))
  })]
};

let neg (\\d -> case {Num (\x -> x.neg)} d);
let add (\\d -> case {Num (\x -> x.add)} d);
let sub (\\d -> case {Num (\x -> x.sub)} d);
let mul (\\d -> case {Num (\x -> x.mul)} d);
let div (\\d -> case {Num (\x -> x.div)} d);
let rem (\\d -> case {Num (\x -> x.rem)} d);

ilet intNum (Num {
  neg negInt
  add addInt
  sub subInt
  mul mulInt
  div divInt
  rem remInt
});
ilet floatNum (Num {
  neg negFloat
  add addFloat
  sub subFloat
  mul mulFloat
  div divFloat
  rem remFloat
});

;; List
type List [t] {
  Nil []
  Cons [t (List t)]
};

let list (arrFoldr (\l x -> Cons x l) Nil);

letr listToArr (case {
  Nil (k [])
  Cons (\h t -> arrAppend [h] (listToArr t))
});

letr listMap (\f -> case {
  Nil (\_ -> Nil)
  Cons (\h t -> Cons (f h) (listMap f t))
});

letr listFoldl (\f v -> case {
  Nil (\_ -> v)
  Cons (\h t -> listFoldl f (f v h) t)
});

letr listFoldr (\f v -> case {
  Nil (\_ -> v)
  Cons (\h t -> f (listFoldr f v t) h)
});

letr listAppend
  (case {Nil (\_ -> id) Cons (\h t x -> Cons h (listAppend t x))});

;; Monoid
type Monoid [t] {
  Monoid [(Rec {
    append (Arr t (Arr t t))
    unit t
  })]
};

let append (\\d -> case {Monoid (\x -> x.append)} d);
let unit (\\d -> case {Monoid (\x -> x.unit)} d);

ilet floatPlusMonoid (Monoid {
  append ((+)@floatNum)
  unit 0.
});
ilet floatMultMonoid (Monoid {
  append ((*)@floatNum)
  unit 1.
});
ilet intPlusMonoid (Monoid {
  append ((+)@intNum)
  unit 0
});
ilet intMultMonoid (Monoid {
  append ((*)@intNum)
  unit 1
});
ilet strMonoid (Monoid {
  append strAppend
  unit ""
});
ilet arrMonoid (Monoid {
  append arrAppend
  unit []
});
ilet listMonoid (Monoid {
  append listAppend
  unit Nil
});

;; Show
type Show [t] {
  Show [(Arr t Str)]
};

let show (\\d -> case {Show id} d);

ilet floatShow (Show floatToString);
ilet intShow (Show intToString);
ilet strShow (Show id);
ilet arrShow (\\showD -> Show $ \a ->
  "[" <> (arrJoin ", " (arrMap (show @ showD) a)) <> "]");
iletr listShow (\\showD -> Show $ \l ->
  (show @ (arrShow @ showD)) (listToArr l));

;; Eq
type Eq [t] {
  Eq [(Arr t (Arr t Bool))]
};

let eq (\\d -> case {Eq id} d);
let neq (\\d -> \x y -> not $ (eq@d) x y);

ilet intEq (Eq (_eq : (Arr Int (Arr Int Bool))));
ilet floatEq (Eq (_eq : (Arr Float (Arr Float Bool))));
ilet strEq (Eq (_eq : (Arr Str (Arr Str Bool))));
ilet arrEq (\\d -> Eq (_arrEq (eq@d)));

;; Functor
type Functor [f] {
  Functor [(Arr (Arr a b) (Arr (f a) (f b)))]
};
let map (\\d -> case {Functor id} d);

ilet arrFunctor (Functor arrMap);
ilet listFunctor (Functor listMap);

;; Foldable
type Foldable [f] {
  Foldable [(Rec {
    foldl (Arr (Arr r (Arr t r)) (Arr r (Arr (f t) r)))
    foldr (Arr (Arr r (Arr t r)) (Arr r (Arr (f t) r)))
  })]
};
let foldl (\\d -> case {Foldable (\x -> x.foldl)} d);
let foldr (\\d -> case {Foldable (\x -> x.foldr)} d);

ilet arrFoldable (Foldable {foldl arrFoldl foldr arrFoldr});
ilet listFoldable (Foldable {foldl listFoldl foldr listFoldr});

;; Ord
type Ord [t] {
  Ord [(Rec {
    gt (Arr t (Arr t Bool))
    geq (Arr t (Arr t Bool))
    lt (Arr t (Arr t Bool))
    leq (Arr t (Arr t Bool))
  })]
};

let gt (\\d -> case {Ord (\x -> x.gt)} d);
let geq (\\d -> case {Ord (\x -> x.geq)} d);
let lt (\\d -> case {Ord (\x -> x.lt)} d);
let leq (\\d -> case {Ord (\x -> x.leq)} d);

ilet intOrd (Ord {
  gt (_gt : (Arr Int (Arr Int Bool)));
  geq (_geq : (Arr Int (Arr Int Bool)));
  lt (_lt : (Arr Int (Arr Int Bool)));
  leq (_leq : (Arr Int (Arr Int Bool)));
});
ilet floatOrd (Ord {
  gt (_gt : (Arr Float (Arr Float Bool)));
  geq (_geq : (Arr Float (Arr Float Bool)));
  lt (_lt : (Arr Float (Arr Float Bool)));
  leq (_leq : (Arr Float (Arr Float Bool)));
});
ilet strOrd (Ord {
  gt (_gt : (Arr Str (Arr Str Bool)));
  geq (_geq : (Arr Str (Arr Str Bool)));
  lt (_lt : (Arr Str (Arr Str Bool)));
  leq (_leq : (Arr Str (Arr Str Bool)));
});

{
  Num Num
  neg neg
  add add
  sub sub
  mul mul
  div div
  rem rem

  implicit_intNum intNum
  implicit_floatNum floatNum

  Nil Nil
  Cons Cons

  list list
  listToArr listToArr
  listMap listMap
  listFoldl listFoldl
  listFoldr listFoldr

  Show Show
  show show

  implicit_floatShow floatShow
  implicit_intShow intShow
  implicit_strShow strShow
  implicit_arrShow arrShow
  implicit_listShow listShow

  Monoid Monoid
  append append
  unit unit

  implicit_floatPlusMonod floatPlusMonoid
  implicit_floatMultMonoid floatMultMonoid
  implicit_intPlusMonod intPlusMonoid
  implicit_intMultMonoid intMultMonoid
  implicit_strMonoid strMonoid
  implicit_arrMonoid arrMonoid
  implicit_listMonoid listMonoid

  Eq Eq
  eq eq
  neq neq

  implicit_intEq intEq
  implicit_floatEq floatEq
  implicit_strEq strEq
  implicit_arrEq arrEq

  Functor Functor
  map map

  implicit_arrFunctor arrFunctor
  implicit_listFunctor listFunctor

  Foldable Foldable
  foldl foldl
  foldr foldr

  implicit_arrFoldable arrFoldable
  implicit_listFoldable listFoldable

  Ord Ord
  gt gt
  lt lt
  leq leq
  geq geq

  implicit_intOrd intOrd
  implicit_floatOrd floatOrd
  implicit_strOrd strOrd
}
