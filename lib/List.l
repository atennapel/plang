type List [t] {
  Nil []
  Cons [t (List t)]
};

let list (arrFoldr (\l x -> Cons x l) Nil);

letr map (\f -> case {
  Nil (\_ -> Nil)
  Cons (\h t -> Cons (f h) (map f t))
});

letr foldl (\f v -> case {
  Nil (\_ -> v)
  Cons (\h t -> foldl f (f v h) t)
});

letr foldr (\f v -> case {
  Nil (\_ -> v)
  Cons (\h t -> f (foldr f v t) h)
});

{
  Nil Nil
  Cons Cons
  list list

  map map
  foldl foldl
  foldr foldr
}
