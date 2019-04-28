-- zero | n * 2 | n * 2 + 1
data BNat = Z | T BNat | TI BNat

n0 = Z
n1 = TI n0
n2 = T n1
n3 = TI n1

toInt :: BNat -> Int
toInt Z = 0
toInt (T n) = 2 * (toInt n)
toInt (TI n) = 1 + 2 * (toInt n)

instance Show BNat where
  show n = show (toInt n)

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

inc :: BNat -> BNat
inc n = foldBNat n (TI Z) (\m _ -> TI m) (\_ r -> T r)

add :: BNat -> BNat -> BNat
add Z n = n
add n Z = n
add (T n) (T m) = T (add n m)
add (T n) (TI m) = TI (add n m)
add (TI n) (T m) = TI (add n m)
add (TI n) (TI m) = T (add (inc n) m)

main :: IO ()
main = putStrLn $ show $ add n3 n3
