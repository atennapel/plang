-- zero | n * 2 | n * 2 + 1
data BNat = Z | T BNat | TI BNat
  deriving (Show)

foldBNat :: BNat -> t -> (BNat -> t -> t) -> (BNat -> t -> t) -> t
foldBNat n z t ti =
  case n of
    Z -> z
    T m -> t m (foldBNat m z t ti)
    TI m -> ti m (foldBNat m z t ti)

div2 :: BNat -> BNat
div2 n = foldBNat n Z (\m _ -> m) (\m _ -> m)

pred :: BNat -> BNat
pred n = foldBNat n Z (\_ r -> TI r) (\m _ -> T m)

succ :: BNat -> BNat
succ n = foldBNat n (TI Z) (\m _ -> TI m) (\_ r -> T r)
