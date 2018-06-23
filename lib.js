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

const _prelude_backup = `
id = \\x -> x;
cnst = \\x y -> x;
flip = \\f x y -> f y x;
comp = \\f g x -> f (g x);
comp2 = \\f g x y -> f (g x y);

data Void;
data Unit = Unit;

data Bool = True | False;
data Nat = Z | S Nat;
data Maybe t = Nothing | Just t;

data Mu (f : Type -> Type) = Mu (forall t. (f t -> t) -> t);
unMu = caseMu id;
cata = \\a l -> unMu l a;

data ListF t r = NilF | ConsF t r;
nil = Mu (\\a -> a NilF);
cons = \\h t -> Mu (\\a -> a (ConsF h (unMu t a)));

data NatF r = ZF | SF r;
z = Mu (\\a -> a ZF);
s = \\n -> Mu (\\a -> a (SF (unMu n a)));
`;

const _prelude = `
id = \\x -> x;
cnst = \\x y -> x;
flip = \\f x y -> f y x;
comp = \\f g x -> f (g x);
comp2 = \\f g x y -> f (g x y);

data Void;
data Unit = Unit;

data Bool = True | False;
data Nat = Z | S Nat;
data Maybe t = Nothing | Just t;
data List t = Nil | Cons t (List t);

inc = S;
add = \\x y -> cataNat x inc y;
pred = caseNat Z id;

length = cataList 0 (\\h a -> S a);
map = \\f -> cataList Nil (\\h a -> Cons (f h) a);
sum = cataList 0 add;
`;
