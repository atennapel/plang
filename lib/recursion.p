import functor

type GR t = (forall r. (r -> r) -> r) -> t
let unGR (GR f) = f

let returnGR x = GR \_ -> x
let joinGR g = GR (\fix -> unGR (unGR g fix) fix)
let mapGR f x = GR \fix -> f (unGR x fix)
let bindGR f x = joinGR (mapGR f x)
let apGR f x = bindGR (\arg -> mapGR (\fn -> fn arg) f) x

let functorGR = Functor mapGR

let fix f = GR \fix -> fix f
let fix1 f x = GR \fix -> fix f x
let fix2 f x y = GR \fix -> fix f x y
let fix3 f x y z = GR \fix -> fix f x y z

let unsafeRunGR (GR f) = f unsafeFix
