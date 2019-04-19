import monoid
import list

type Char = Nat
let fromChar (Char n) = n
let liftChar f (Char a) = Char (f a)
let liftChar2 f (Char a) (Char b) = Char (f a b)
let liftChar3 f (Char a) (Char b) (Char c) = Char (f a b c)

type Str = List Char
let emptyStr = Str nil
let fromStr (Str l) = l
let liftStr f (Str a) = Str (f a)
let liftStr2 f (Str a) (Str b) = Str (f a b)
let liftStr3 f (Str a) (Str b) (Str c) = Str (f a b c)

let appendStr = liftStr2 append

let monoidStr = monoid emptyStr appendStr
