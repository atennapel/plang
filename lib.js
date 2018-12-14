function show(x) {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag :
      Array.isArray(x.val) ? `(${x._tag} ${x.val.map(show).join(' ')})` :
      `(${x._tag} ${show(x.val)})`;
  if (x._cont) return `${x.op}(${show(x.val)})`;
  return '' + x;
}

function caseVoid(x) {
  throw new Error('Void');
}

const id = x => x;
const cnst = x => y => x;

const Unit = { _tag: 'Unit' };
const True = { _tag: 'True' };
const False = { _tag: 'False' };
const caseBool = a => b => x => x._tag === 'True' ? a : b;
const not = b => b._tag === 'True' ? False : True;

const Pair = a => b => ({ _tag: 'Pair', val: [a, b] });
const fst = p => p.val[0];
const snd = p => p.val[1];

const Z = { _tag: 'Z' };
const S = x => ({ _tag: 'S', val: x });
const caseNat = z => s => x => x._tag === 'S' ? $(s, x.val) : z;

const Nothing = { _tag: 'Nothing' };
const Just = x => ({ _tag: 'Just', val: x });
const caseMaybe = z => s => x => x._tag === 'Just' ? $(s, x.val) : z;

const Nil = { _tag: 'Nil' };
const Cons = h => t => ({ _tag: 'Cons', val: [h, t] });
const caseList = z => s => x => x._tag === 'Cons' ? $($(s, x.val[0]), x.val[1]) : z; 

const fix = f => $(x => $(f, y => $($(x, x), y)), x => $(f, y => $($(x, x), y)));

// effects
const _cont = (op, val, cont) => ({ _cont: true, op, val, cont });
const _op = op => val => _cont(op, val, x => x);
const _do = (c, f) => c._cont ? _cont(c.op, c.val, v => _do(c.cont(v), f)) : f(c);
const _handler = m => c =>
  c._cont? (m[c.op]? m[c.op](c.val)(v => _handler(m)(c.cont(v))): _cont(c.op, c.val, v => _handler(m)(c.cont(v)))):
  m['return']? m['return'](c): c;

const _newhandler = m => {
  const r = [];
  for (let k in m) r.push([k, m[k]]);
  return _do(r.reduce((p, [k, c]) =>
    _do(c, x => Object.assign(p, { [k]: x })), {}),
    m => c => _handler(m)(c(Unit)));
};

const $ = (f, x) => _do(f, f => _do(x, x => f(x)));

const flip = _op('flip');
const fail = _op('fail');
const get = _op('get');
const put = _op('put');
