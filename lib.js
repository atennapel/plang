function show(x) {
  if(x._adt) {
    if(x._tag === 'Z') return '0';
    if(x._tag === 'S') {
      let c = x;
      let n = 0;
      while(c._tag === 'S') {
        n++;
        c = c._args[0];
      }
      return `${n}`;
    }
    if(x._tag === 'Nil') return '[]';
    if(x._tag === 'Cons') {
      let c = x;
      let r = [];
      while(c._tag === 'Cons') {
        r.push(c._args[0]);
        c = c._args[1];
      }
      return '[' + r.map(show).join(', ') + ']';
    }
    return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  }
  if(typeof x === 'function') return `[Function]`;
  return `${x}`;
}

const emptyStr = "";
const appendStr = x => y => x + y;

const zeroFloat = 0;
const oneFloat = 1;
const negFloat = x => -x;
const incFloat = x => x + 1;
const decFloat = x => x - 1;
const addFloat = x => y => x + y;
const subFloat = x => y => x - y;
const mulFloat = x => y => x * y;
const divFloat = x => y => x / y;
const modFloat = x => y => x % y;

const _prelude = `
id = \\x -> x;
cnst = \\x y -> x;
flip = \\f x y -> f y x;
comp = \\f g x -> f (g x);
comp2 = \\f g x y -> f (g x y);

data Void;
data Unit = Unit;

data Pair a b = Pair a b;
fst = casePair (\\a b -> a);
snd = casePair (\\a b -> b);

data Either a b = Left a | Right b;

data Bool = True | False;
data Nat = Z | S Nat;
data Maybe t = Nothing | Just t;
data List t = Nil | Cons t (List t);

not = caseBool False True;
and = caseBool id (\\x -> False);
or = caseBool (\\x -> True) id;

inc = S;
add = \\x y -> cataNat x inc y;
pred = caseNat Z id;

length = cataList 0 (\\h a -> S a);
map = \\f -> cataList Nil (\\h a -> Cons (f h) a);
sum = cataList 0 add;
`;
