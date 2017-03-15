let Nil (pack List (inj Nil ())) (
let Cons (fn h (fn t (pack List (inj Cons (extend 0 h (extend 1 t ())))))) (
letr sum
  (fn l
    (elim Nil (fn x zero)
    (elim Cons (fn t (add (sel 0 t) (sum (sel 1 t)))) end)
    (unpack List l))) (sum (Cons one (Cons one (Cons one Nil))))
))
