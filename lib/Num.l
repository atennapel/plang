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
}
