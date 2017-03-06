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
  append (+)
  unit 0
});
ilet floatMultMonoid (Monoid {
  append (*)
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
ilet strShow (Show id);
ilet arrShow (\\showD -> Show $ \a ->
  "[" <> (arrJoin ", " (arrMap (show @ showD) a)) <> "]");
iletr listShow (\\showD -> Show $ \l ->
  (show @ (arrShow @ showD)) (listToArr l));

{
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
  implicit_strShow strShow
  implicit_arrShow arrShow
  implicit_listShow listShow

  Monoid Monoid
  append append
  unit unit

  implicit_floatPlusMonod floatPlusMonoid
  implicit_floatMultMonoid floatMultMonoid
  implicit_strMonoid strMonoid
  implicit_arrMonoid arrMonoid
  implicit_listMonoid listMonoid
}
