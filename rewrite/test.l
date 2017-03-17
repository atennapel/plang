let Nil (pack List ; inj Nil ());
let Cons (fn h t; pack List ; inj Cons (extend 0 h ; extend 1 t ()));

letr map (fn f l; (
  elim Nil (fn x; Nil);
  elim Cons (fn t; Cons (f (sel 0 t)) (map f (sel 1 t)));
  end
) (unpack List l));

map (add one) (Cons one; Cons one; Cons one Nil)
