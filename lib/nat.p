; parigot-encoded unary natural numbers
import basic

type Nat = forall t. (() -> t) -> (Nat -> t -> t) -> t
