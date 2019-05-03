import functor

type Monad m = forall r.
  (Functor m ->
  (forall (t:Type). t -> m t) ->
  (forall (a b:Type). (a -> m b) -> m a -> m b) -> r) -> r
let caseMonad (Monad f) = f

let functorFromMonad m = caseMonad m \f _ _ -> f
let return m = caseMonad m \_ r _ -> r
let bind m = caseMonad m \_ _ b -> b
