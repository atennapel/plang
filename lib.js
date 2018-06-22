function show(x) {
  if(x === null) return `()`;
  if(x._adt) return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  if(Array.isArray(x)) return `[${x.map(show).join(', ')}]`;
  if(typeof x === 'function') return `[Function]`;
  if(x._tag === 'inl') return `(Inl ${show(x._val)})`;
  if(x._tag === 'inr') return `(Inr ${show(x._val)})`;
  if(x._tag === 'pair') return `(${show(x._fst)}, ${show(x._snd)})`;
  return `${x}`;
}

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
