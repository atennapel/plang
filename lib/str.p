import monoid
import list

type Char = Nat
let fromChar (Char n) = n

type Str = List Char
let emptyStr = Str nil
let fromStr (Str l) = l
let strLift2 f (Str a) (Str b) = Str (f a b)
let strAppend = strLift2 append

let monoidStr = monoid emptyStr strAppend
