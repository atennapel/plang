function show(x) {
  if(x._rec) {
    if(x.length === 0) return '{}';
    const r = [];
    for(let i = x.length - 1; i >= 0; i--)
      r.push(`${x[i][0]} = ${show(x[i][1])}`);
    return `{ ${r.join(', ')} }`;
  }
  if(x._eff) {
    if(x.tag === 'ret') return `!(${show(x.val)})`;
    if(x.tag === 'cont') return `!(${x.op} ${show(x.val)})`;
  }
  if(x._var) {
    let l = '';
    for(let i = 0; i < x.level; i++) l += '^';
    return `(${l}${x.label} ${show(x.val)})`;
  }
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

const _recEmpty = [];
_recEmpty._rec = true;
const _recSelect = l => r => {
  for(let i = r.length - 1; i >= 0; i--) {
    const c = r[i];
    if(c[0] === l) return c[1];
  }
  throw new Error(`${l} not found in record`);
};
const _recExtend = l => v => r => {
  const r_ = r.concat([[l, v]]);
  r_._rec = true;
  return r_;
};
const _recRestrict = l => r => {
  const r_ = [];
  let found = false;
  for(let i = r.length - 1; i >= 0; i--) {
    const c = r[i];
    if(!found && c[0] === l) {
      found = true;
    } else r_.push(c);
  }
  r_._rec = true;
  return r_.reverse();
};
const _recSet = l => v => r => {
  const r_ = [];
  let found = false;
  for(let i = r.length - 1; i >= 0; i--) {
    const c = r[i];
    if(!found && c[0] === l) {
      found = true;
      r_.push([l, v]);
    } else r_.push(c);
  }
  r_._rec = true;
  return r_.reverse();
};
const _recUpdate = l => f => r => {
  const r_ = [];
  let found = false;
  for(let i = r.length - 1; i >= 0; i--) {
    const c = r[i];
    if(!found && c[0] === l) {
      found = true;
      r_.push([l, f(c[1])]);
    } else r_.push(c);
  }
  r_._rec = true;
  return r_.reverse();
};

const _varEmpty = () => { throw new Error('empty variant') };
const _varInject = label => val => ({ _var: true, label, val, level: 0 });
const _varEmbed = l => v => v.label === l? ({ _var: true, label: l, val: v.val, level: v.level + 1 }): v;
const _varCase = l => r => fa => fb => r.label === l? (r.level === 0? fa(r.val): fb({ _var: true, label: l, val: r.val, level: r.level - 1 })): fb(r);
const _varUpdate = l => f => v => v.label === l && v.level === 0? { _var: true, label: l, level: 0, val: f(v.val) }: v;

const _return = val => ({ _eff: true, tag: 'ret', val });
const _cont = (op, val, cont) => ({ _eff: true, tag: 'cont', op, val, cont });
const _pure = x => x.val;
const _op = op => val => _cont(op, val, _return);

const _do = c => f =>
  c.tag === 'ret'? f(c.val):
  c.tag === 'cont'? _cont(c.op, c.val, v => _do(c.cont(v))(f)):
  null;

const _handler = m => c => 
  c.tag === 'ret'? (m['return']? m['return'](c.val): c):
  c.tag === 'cont'? (m[c.op]? m[c.op](c.val)(v => _handler(m)(c.cont(v))): _cont(c.op, c.val, v => _handler(m)(c.cont(v)))):
  null;
const _phandler = m => iv => c => 
  c.tag === RET? (m['return']? m['return'](iv)(c.val): c):
  c.tag === CONT? (m[c.op]? m[c.op](iv)(c.val)((iv, v) => _phandler(m)(iv)(c.cont(v))): _cont(c.op, c.val, v => _phandler(m)(iv)(c.cont(v)))):
  null;

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
id = \\x -> x;;
cnst = \\x y -> x;;
flip = \\f x y -> f y x;;
comp = \\f g x -> f (g x);;
comp2 = \\f g x y -> f (g x y);;
app = \\f x -> f x;;

data Void;;
data Unit = Unit;;

data Pair a b = Pair a b;;
fst = casePair (\\a b -> a);;
snd = casePair (\\a b -> b);;

data Either a b = Left a | Right b;;

data Bool = True | False;;
data Nat = Z | S Nat;;
data Maybe t = Nothing | Just t;;
data List t = Nil | Cons t (List t);;

not = caseBool False True;;
and = caseBool id (\\x -> False);;
or = caseBool (\\x -> True) id;;

inc = S;;
add = \\x y -> cataNat x inc y;;
pred = caseNat Z id;;

length = cataList 0 (\\h a -> S a);;
map = \\f -> cataList Nil (\\h a -> Cons (f h) a);;
sum = cataList 0 add;;
`;
