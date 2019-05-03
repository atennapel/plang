import str
import functor
import monad

type IO t = forall r. (t -> r) -> ((Str -> IO t) -> r) -> (Str -> IO t -> r) -> r
let caseIO (IO f) = f

let returnIO x = IO \r g p -> r x
let getLine c = IO \r g p -> g c
let putLine s c = IO \r g p -> p s c

let mapIO f = unsafeFix \rec io ->
  caseIO io
    (\x -> returnIO (f x))
    (\c -> getLine \x -> rec (c x))
    (\s c -> putLine s (rec c))
let functorIO = Functor mapIO

let bindIO f = unsafeFix \rec io ->
  caseIO io
    f
    (\c -> getLine \x -> rec (c x))
    (\s c -> putLine s (rec c))

let monadIO = Monad \f -> f functorIO returnIO bindIO
