import monoid
import eq
import nat
import list

type Char = Nat
let fromChar (Char n) = n
let liftChar f (Char a) = Char (f a)
let liftChar2 f (Char a) (Char b) = Char (f a b)
let liftChar3 f (Char a) (Char b) (Char c) = Char (f a b c)

let eqChar = (let eqc (Char a) (Char b) = eq eqNat a b in Eq eqc)

type Str = List Char
let emptyStr = Str nil
let fromStr (Str l) = l
let liftStr f (Str a) = Str (f a)
let liftStr2 f (Str a) (Str b) = Str (f a b)
let liftStr3 f (Str a) (Str b) (Str c) = Str (f a b c)

let appendStr = liftStr2 append
let eqStr = (let eql (Str a) (Str b) = eq (eqList eqChar) a b in Eq eql)

let monoidStr = monoid emptyStr appendStr
