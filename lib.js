function show(x) {
  if (typeof x === 'string') return JSON.stringify(x);
  if (typeof x === 'function') return '[Function]';
  if (typeof x._tag === 'string')
    return typeof x.val === 'undefined' ? x._tag : `(${x._tag} ${show(x.val)})`;
  if (x._cont) return `(${x.op}(${show(x.val)}))`;
  return '' + x;
}

function caseVoid(x) {
  throw new Error('Void');
}

const Unit = { _tag: 'Unit' };
const True = { _tag: 'True' };
const False = { _tag: 'False' };
const caseBool = a => b => x => x._tag === 'True' ? a : b;

const Z = { _tag: 'Z' };
const S = x => ({ _tag: 'S', val: x });
const caseNat = z => s => x => x._tag === 'S' ? s(x.val) : z;

const Nothing = { _tag: 'Nothing' };
const Just = x => ({ _tag: 'Just', val: x });
const caseMaybe = z => s => x => x._tag === 'Just' ? s(x.val) : z;

const fix = f => (x => f(y => x(x)(y)))(x => f(y => x(x)(y)));

// effects
const _cont = (op, val, cont) => ({ _cont: true, op, val, cont });
const _op = op => val => _cont(op, val, x => x);
const _do = (c, f) => c._cont ? _cont(c.op, c.val, v => _do(c.cont(v), f)) : f(c);
const _handler = m => c =>
  c._cont? (m[c.op]? m[c.op](c.val)(v => _handler(m)(c.cont(v))): _cont(c.op, c.val, v => _handler(m)(c.cont(v)))):
  m['return']? m['return'](c): c;

const flip = _op('flip');
const fail = _op('fail');

const runFlip = c => _handler({
  flip: v => k => k(Math.random() > 0.5 ? True : False),
})(c(Unit));
const runFail = c => _handler({
  fail: v => k => Nothing,
  return: Just,
})(c(Unit));
