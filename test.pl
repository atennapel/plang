data Void;

data Nat = Z | S Nat;

data List t = Nil | Cons t (List t);

data A (f : Type -> Type) = A (forall t. t -> f t);

main = A (\x -> Cons x Nil);
