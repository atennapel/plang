type Functor f = forall a b. (a -> b) -> f a -> f b
let map (Functor f) = f
