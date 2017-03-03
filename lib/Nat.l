type Nat [] {
  Z []
  S [Nat]
};

letr fromFloat (\n -> if (n <= 0) Z (S $ fromFloat $ n - 1));

letr toFloat (case {
  Z (k 0)
  S (\n -> 1 + (toFloat n))
});

{
  Z Z
  S S

  fromFloat fromFloat
  toFloat toFloat
}
