newtype IState i o a = IState { runIState :: i -> (o, a) }

class IMonad m where
  ireturn :: a -> m i i a
  (>>>=) :: m i j a -> (a -> m j k b) -> m i k b

(>>>) :: IMonad m => m i j a -> m j k b -> m i k b
mx >>> my = mx >>>= const my

instance IMonad IState where
  ireturn x = IState $ \s -> (s, x)
  IState f >>>= g = IState $ \i -> let (o, x) = f i
                                   in runIState (g x) o

get :: IState s s s
get = IState $ \s -> (s, s)

put :: s -> IState i s ()
put x = IState $ \_ -> (x, ())

myStateComputation :: IState Int String (Int, Bool, String)
myStateComputation =
    -- poor man's do notation. You could use RebindableSyntax
    get              >>>= \s ->
    put (s > 10)     >>>
    get              >>>= \t ->
    put (show s)     >>>
    get              >>>= \v ->
    ireturn (s, t, v)

main = print $ runIState myStateComputation 3
